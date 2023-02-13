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
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import io.micronaut.maven.DockerfileMojo;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
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
    private final MavenProject mavenProject;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerService(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
        DockerClientConfig config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
        DockerHttpClient httpClient = new ZerodepDockerHttpClient.Builder()
                .dockerHost(config.getDockerHost())
                .sslConfig(config.getSSLConfig())
                .build();
        dockerClient = DockerClientImpl.getInstance(config, httpClient);
    }

    /**
     * Creates the {@link BuildImageCmd} by loading the given Dockerfile as classpath resource.
     */
    public BuildImageCmd buildImageCmd(String dockerfileName) throws IOException {
        return dockerClient.buildImageCmd(loadDockerfileAsResource(dockerfileName));
    }

    /**
     * Creates a default {@link BuildImageCmd}.
     */
    public BuildImageCmd buildImageCmd() {
        return dockerClient.buildImageCmd();
    }

    /**
     * Builds the Docker image from the given {@link BuildImageCmd} builder.
     *
     * @return The resulting image ID.
     */
    public String buildImage(BuildImageCmd builder) {
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
     * @param imageId the image to use
     * @param timeoutSeconds the timeout in seconds for the container to finish execution
     * @param binds the bind mounts to use
     */
    public void runPrivilegedImageAndWait(String imageId, Integer timeoutSeconds, String checkpointNetworkName, String... binds) throws IOException {
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
                    Integer exitcode = waitResult.awaitStatusCode(timeoutSeconds, TimeUnit.SECONDS);
                    if (exitcode != 0) {
                        throw new IOException("Image " + imageId + " exited with code " + exitcode);
                    }
                }
            }
        }
    }

    /**
     * Copies a file from the specified container path in the given image ID, into a temporal location.
     */
    public File copyFromContainer(String imageId, String containerPath) {
        CreateContainerCmd containerCmd = dockerClient.createContainerCmd(imageId);
        CreateContainerResponse container = containerCmd.exec();
        dockerClient.startContainerCmd(container.getId());
        InputStream nativeImage = dockerClient.copyArchiveFromContainerCmd(container.getId(), containerPath).exec();

        try (TarArchiveInputStream fin = new TarArchiveInputStream(nativeImage)) {
            TarArchiveEntry tarEntry = fin.getNextTarEntry();
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
     */
    public File loadDockerfileAsResource(String name) throws IOException {
        return loadDockerfileAsResource(name, DockerfileMojo.DOCKERFILE);
    }

    /**
     * Loads the given Dockerfile as classpath resource and copies it into a temporary location in the target directory.
     */
    public File loadDockerfileAsResource(String name, String targetFileName) throws IOException {
        String path = "/dockerfiles/" + name;
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) {
            File dockerfile = new File(mavenProject.getBuild().getDirectory(), targetFileName);
            FileUtils.copyInputStreamToFile(stream, dockerfile);
            return dockerfile;
        }
        return null;
    }

    /**
     * Creates a {@link PushImageCmd} from the given image name.
     */
    public PushImageCmd pushImageCmd(String imageName) {
        return dockerClient.pushImageCmd(imageName);
    }
}
