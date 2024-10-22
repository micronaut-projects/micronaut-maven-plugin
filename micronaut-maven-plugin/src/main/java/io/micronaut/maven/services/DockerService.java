/*
 * Copyright 2017-2022 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven.services;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.PushImageCmd;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.command.WaitContainerCmd;
import com.github.dockerjava.api.command.WaitContainerResultCallback;
import com.github.dockerjava.api.exception.DockerClientException;
import com.github.dockerjava.api.exception.DockerException;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.AuthConfigurations;
import com.github.dockerjava.api.model.AuthResponse;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import com.google.cloud.tools.jib.api.Credential;
import io.micronaut.maven.DockerfileMojo;
import io.micronaut.maven.jib.JibConfigurationService;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.output.FrameConsumerResultCallback;
import org.testcontainers.containers.output.OutputFrame;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.utility.RegistryAuthLocator;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Provides methods to work with Docker images.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class DockerService {

    private static final Logger LOG = LoggerFactory.getLogger(DockerService.class);

    private final DockerClient dockerClient;
    private final DockerClientConfig config;
    private final MavenProject mavenProject;
    private final JibConfigurationService jibConfigurationService;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerService(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        this.config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
        var httpClient = new ZerodepDockerHttpClient.Builder()
            .dockerHost(config.getDockerHost())
            .sslConfig(config.getSSLConfig())
            .build();
        dockerClient = DockerClientImpl.getInstance(config, httpClient);
    }

    /**
     * @param dockerfileName the name of the Dockerfile to load
     * @return the {@link BuildImageCmd} by loading the given Dockerfile as classpath resource.
     */
    public BuildImageCmd buildImageCmd(String dockerfileName) throws IOException {
        verifyDockerRunning();
        BuildImageCmd buildImageCmd = dockerClient.buildImageCmd(loadDockerfileAsResource(dockerfileName));
        maybeConfigureBuildAuth(buildImageCmd);
        return buildImageCmd;
    }

    private void maybeConfigureBuildAuth(BuildImageCmd buildImageCmd) {
        Optional<String> fromImage = jibConfigurationService.getFromImage();
        Optional<Credential> fromCredentials = jibConfigurationService.getFromCredentials();
        if (fromImage.isPresent() && fromCredentials.isPresent()) {
            AuthConfig authConfig = getAuthConfigFor(fromImage.get(), fromCredentials.get().getUsername(), fromCredentials.get().getPassword());
            var authConfigurations = new AuthConfigurations();
            authConfigurations.addConfig(authConfig);
            buildImageCmd.withBuildAuthConfigs(authConfigurations);
        }
    }

    /**
     * @return a default {@link BuildImageCmd}.
     */
    public BuildImageCmd buildImageCmd() {
        verifyDockerRunning();
        BuildImageCmd buildImageCmd = dockerClient.buildImageCmd();
        maybeConfigureBuildAuth(buildImageCmd);
        return buildImageCmd;
    }

    /**
     * Builds the Docker image from the given {@link BuildImageCmd} builder.
     *
     * @param builder The builder to use.
     * @return The resulting image ID.
     */
    public String buildImage(BuildImageCmd builder) {
        verifyDockerRunning();
        builder.getBuildArgs().forEach((k, v) -> LOG.info("Using {}: {}", k, v));
        BuildImageResultCallback resultCallback = new BuildImageResultCallback() {
            @Override
            public void onNext(BuildResponseItem item) {
                super.onNext(item);
                if (item.isErrorIndicated() && item.getErrorDetail() != null) {
                    LOG.error(item.getErrorDetail().getMessage());
                } else if (item.getStream() != null) {
                    String msg = StringUtils.removeEnd(item.getStream(), System.lineSeparator());
                    LOG.info(msg);
                }
            }
        };

        return builder
            .exec(resultCallback)
            .awaitImageId();
    }

    /**
     * Creates a container based on a given image, and runs it.
     *
     * @param imageId the image to use
     * @param timeoutSeconds the timeout in seconds for the container to finish execution
     * @param checkpointNetworkName the name of the network to use for the container
     * @param binds the bind mounts to use
     */
    public void runPrivilegedImageAndWait(String imageId, Integer timeoutSeconds, String checkpointNetworkName, String... binds) throws IOException {
        verifyDockerRunning();
        try (CreateContainerCmd create = dockerClient.createContainerCmd(imageId)) {
            HostConfig hostConfig = create.getHostConfig();
            if (hostConfig == null) {
                throw new DockerClientException("When setting binds and privileged, hostConfig was null.  Please check your docker installation and try again");
            }
            hostConfig.withPrivileged(true);
            if (checkpointNetworkName != null) {
                hostConfig.withNetworkMode(checkpointNetworkName);
            }
            for (String bind : binds) {
                hostConfig.withBinds(Bind.parse(bind));
            }
            CreateContainerResponse createResponse = create.exec();
            try (StartContainerCmd start = dockerClient.startContainerCmd(createResponse.getId())) {
                start.exec();
                LOG.info("Container started: {} {}", createResponse.getId(), start.getContainerId());
                try (WaitContainerCmd wait = dockerClient.waitContainerCmd(createResponse.getId())) {
                    WaitContainerResultCallback waitResult = wait.start();
                    LOG.info("Waiting {} seconds for completion", timeoutSeconds);
                    Integer exitCode = waitResult.awaitStatusCode(timeoutSeconds, TimeUnit.SECONDS);
                    if (exitCode != 0) {
                        final Slf4jLogConsumer stdoutConsumer = new Slf4jLogConsumer(LOG);
                        final Slf4jLogConsumer stderrConsumer = new Slf4jLogConsumer(LOG);

                        try (var callback = new FrameConsumerResultCallback()) {
                            callback.addConsumer(OutputFrame.OutputType.STDOUT, stdoutConsumer);
                            callback.addConsumer(OutputFrame.OutputType.STDERR, stderrConsumer);

                            dockerClient.logContainerCmd(start.getContainerId())
                                .withStdOut(true)
                                .withStdErr(true)
                                .exec(callback)
                                .awaitCompletion();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                        throw new IOException("Image " + imageId + " exited with code " + exitCode);
                    }
                }
            }
        }
    }

    /**
     * Copies a file from the specified container path in the given image ID, into a temporal location.
     *
     * @param imageId The image ID.
     * @param containerPath The container path.
     * @return The temporal file.
     */
    public File copyFromContainer(String imageId, String containerPath) {
        CreateContainerCmd containerCmd = dockerClient.createContainerCmd(imageId);
        CreateContainerResponse container = containerCmd.exec();
        dockerClient.startContainerCmd(container.getId());
        InputStream nativeImage = dockerClient.copyArchiveFromContainerCmd(container.getId(), containerPath).exec();

        try (var fin = new TarArchiveInputStream(nativeImage)) {
            TarArchiveEntry tarEntry = fin.getNextEntry();
            File file = new File(mavenProject.getBuild().getDirectory(), tarEntry.getName());
            if (!file.getCanonicalFile().toPath().startsWith(mavenProject.getBuild().getDirectory())) {
                throw new IOException("Entry is outside of the target directory");
            }

            IOUtils.copy(fin, Files.newOutputStream(file.toPath()));

            return file;
        } catch (IOException e) {
            LOG.error("Failed to copy file from container", e);
        } finally {
            containerCmd.close();
        }
        return null;
    }

    /**
     * Loads the given Dockerfile as classpath resource and copies it into a temporary location in the target directory.
     *
     * @param name the name of the Dockerfile.
     * @return the file where the Dockerfile was copied to.
     */
    public File loadDockerfileAsResource(String name) throws IOException {
        return loadDockerfileAsResource(name, DockerfileMojo.DOCKERFILE);
    }

    /**
     * Loads the given Dockerfile as classpath resource and copies it into a temporary location in the target directory.
     *
     * @param name the name of the Dockerfile.
     * @param targetFileName the name of the file to copy the Dockerfile to.
     * @return the file where the Dockerfile was copied to.
     */
    public File loadDockerfileAsResource(String name, String targetFileName) throws IOException {
        String path = "/dockerfiles/" + name;
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) {
            var dockerfile = new File(mavenProject.getBuild().getDirectory(), targetFileName);
            FileUtils.copyInputStreamToFile(stream, dockerfile);
            return dockerfile;
        }
        return null;
    }

    /**
     * @param imageName the image name
     * @return a {@link PushImageCmd} from the given image name.
     */
    public PushImageCmd pushImageCmd(String imageName) {
        verifyDockerRunning();
        return dockerClient.pushImageCmd(imageName);
    }

    /**
     * @param dockerImage the image name
     * @param username the username
     * @param password the password
     * @return an {@link AuthConfig} object for the given image, username and password.
     */
    public AuthConfig getAuthConfigFor(String dockerImage, String username, String password) {
        DockerImageName dockerImageName = DockerImageName.parse(dockerImage);
        var defaultAuthConfig = new AuthConfig()
            .withRegistryAddress(dockerImageName.getRegistry())
            .withUsername(username)
            .withPassword(password);
        RegistryAuthLocator registryAuthLocator = RegistryAuthLocator.instance();
        AuthConfig authConfig = registryAuthLocator.lookupAuthConfig(dockerImageName, defaultAuthConfig);
        AuthResponse authResponse = dockerClient.authCmd().withAuthConfig(authConfig).exec();
        if (authResponse.getStatus() != null && authResponse.getStatus().equals("Login Succeeded")) {
            LOG.info("Successfully logged in to registry {}", dockerImageName.getRegistry());
        } else {
            LOG.warn("Failed to login to registry {}", dockerImageName.getRegistry());
        }
        return authConfig;
    }

    private void verifyDockerRunning() {
        try {
            dockerClient.pingCmd().exec();
        } catch (DockerException e) {
            throw new IllegalStateException(e.getMessage());
        } catch (RuntimeException e) {
            throw new IllegalStateException("Cannot connect to the Docker daemon at " + config.getDockerHost() + ". Is the docker daemon running?", e);
        }
    }
}
