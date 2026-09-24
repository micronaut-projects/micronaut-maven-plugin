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
import com.github.dockerjava.api.async.ResultCallback;
import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.ExecCreateCmd;
import com.github.dockerjava.api.command.InspectImageCmd;
import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.command.KillContainerCmd;
import com.github.dockerjava.api.command.PushImageCmd;
import com.github.dockerjava.api.command.RemoveContainerCmd;
import com.github.dockerjava.api.command.RemoveImageCmd;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.command.WaitContainerCmd;
import com.github.dockerjava.api.command.WaitContainerResultCallback;
import com.github.dockerjava.api.exception.DockerClientException;
import com.github.dockerjava.api.exception.ConflictException;
import com.github.dockerjava.api.exception.DockerException;
import com.github.dockerjava.api.exception.NotFoundException;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.AuthConfigurations;
import com.github.dockerjava.api.model.AuthResponse;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.api.model.Frame;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.Info;
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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * Provides methods to work with Docker images.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class DockerService {

    private static final Logger LOG = LoggerFactory.getLogger(DockerService.class);

    private final DockerClientConfig config;
    private final MavenProject mavenProject;
    private final JibConfigurationService jibConfigurationService;
    private DockerClient dockerClient;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerService(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        this.config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
    }

    /**
     * Uses the given Docker client instead of connecting to the configured Docker host. For tests.
     *
     * @param mavenProject the Maven project
     * @param jibConfigurationService the Jib configuration service
     * @param dockerClient the Docker client
     */
    DockerService(MavenProject mavenProject, JibConfigurationService jibConfigurationService, DockerClient dockerClient) {
        this(mavenProject, jibConfigurationService);
        this.dockerClient = dockerClient;
    }

    private DockerClient getDockerClient() {
        if (dockerClient == null) {
            var httpClient = new ZerodepDockerHttpClient.Builder()
                    .dockerHost(config.getDockerHost())
                    .sslConfig(config.getSSLConfig())
                    .build();
            dockerClient = DockerClientImpl.getInstance(config, httpClient);
        }
        return dockerClient;
    }

    /**
     * @param dockerfileName the name of the Dockerfile to load
     * @return the {@link BuildImageCmd} by loading the given Dockerfile as classpath resource.
     */
    public BuildImageCmd buildImageCmd(String dockerfileName) throws IOException {
        verifyDockerRunning();
        BuildImageCmd buildImageCmd = getDockerClient().buildImageCmd(loadDockerfileAsResource(dockerfileName));
        maybeConfigureBuildAuth(buildImageCmd);
        return buildImageCmd;
    }

    private void maybeConfigureBuildAuth(BuildImageCmd buildImageCmd) {
        jibConfigurationService.getFromImage().ifPresent(image -> {
            Optional<Credential> fromCredentials = jibConfigurationService.getFromCredentials();
            Optional<Credential> credential = fromCredentials.or(() -> jibConfigurationService.resolveCredentialForImage(image, LOG));
            credential.ifPresent(cred -> {
                var username = cred.getUsername();
                var password = cred.getPassword();
                AuthConfig authConfig = getAuthConfigFor(image, username, password);
                var authConfigurations = new AuthConfigurations();
                authConfigurations.addConfig(authConfig);
                buildImageCmd.withBuildAuthConfigs(authConfigurations);
            });
        });
    }

    /**
     * @return a default {@link BuildImageCmd}.
     */
    public BuildImageCmd buildImageCmd() {
        verifyDockerRunning();
        BuildImageCmd buildImageCmd = getDockerClient().buildImageCmd();
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
        if (builder.getBuildArgs() != null) {
            builder.getBuildArgs().forEach((k, v) -> LOG.info("Using {}: {}", k, v));
        }
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
        String containerId = createContainer(imageId, checkpointNetworkName, true, Map.of(), binds);
        startAndWait(containerId, imageId, timeoutSeconds);
    }

    /**
     * Creates a container from the given image, without starting it.
     *
     * @param imageId the image to use
     * @param networkName the name of the network to use for the container, if any
     * @param privileged whether the container is privileged
     * @param environment the environment variables to set, on top of the image ones
     * @param binds the bind mounts to use
     * @return the container ID
     * @since 5.1.0
     */
    public String createContainer(String imageId, String networkName, boolean privileged, Map<String, String> environment,
                                  String... binds) {
        verifyDockerRunning();
        try (CreateContainerCmd create = getDockerClient().createContainerCmd(imageId)) {
            HostConfig hostConfig = create.getHostConfig();
            if (hostConfig == null) {
                throw new DockerClientException("When setting binds and privileged, hostConfig was null.  Please check your docker installation and try again");
            }
            if (privileged) {
                hostConfig.withPrivileged(true);
            }
            if (networkName != null) {
                hostConfig.withNetworkMode(networkName);
            }
            for (String bind : binds) {
                hostConfig.withBinds(Bind.parse(bind));
            }
            if (!environment.isEmpty()) {
                create.withEnv(environment.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue()).toList());
            }
            return create.exec().getId();
        }
    }

    /**
     * Starts a container and waits for it to exit with status 0. Otherwise, logs its output and fails.
     *
     * @param containerId the container
     * @param imageId the image of the container, for the error message
     * @param timeoutSeconds the timeout in seconds for the container to finish execution
     * @throws IOException if the container does not exit with status 0 within the timeout
     * @since 5.1.0
     */
    public void startAndWait(String containerId, String imageId, Integer timeoutSeconds) throws IOException {
        startContainer(containerId);
        LOG.info("Waiting {} seconds for completion", timeoutSeconds);
        int exitCode;
        try {
            exitCode = awaitExit(containerId, timeoutSeconds);
        } catch (IOException e) {
            logContainerOutput(containerId);
            throw e;
        }
        if (exitCode != 0) {
            logContainerOutput(containerId);
            throw new IOException("Image " + imageId + " exited with code " + exitCode);
        }
    }

    /**
     * Starts a container.
     *
     * @param containerId the container
     * @since 5.1.0
     */
    public void startContainer(String containerId) {
        try (StartContainerCmd start = getDockerClient().startContainerCmd(containerId)) {
            start.exec();
            LOG.info("Container started: {}", containerId);
        }
    }

    /**
     * Waits for a container to exit.
     *
     * @param containerId the container
     * @param timeoutSeconds the timeout in seconds
     * @return the exit code of the container
     * @throws IOException if the container does not exit within the timeout
     * @since 5.1.0
     */
    public int awaitExit(String containerId, int timeoutSeconds) throws IOException {
        try (WaitContainerCmd wait = getDockerClient().waitContainerCmd(containerId)) {
            WaitContainerResultCallback waitResult = wait.start();
            try {
                return waitResult.awaitStatusCode(timeoutSeconds, TimeUnit.SECONDS);
            } catch (DockerClientException e) {
                throw new IOException("Container " + containerId + " did not exit within " + timeoutSeconds + " seconds", e);
            }
        }
    }

    /**
     * Sends a signal to the main process of a running container.
     *
     * @param containerId the container
     * @param signal the signal, for example {@code SIGTERM}
     * @since 5.1.0
     */
    public void signalContainer(String containerId, String signal) {
        try (KillContainerCmd kill = getDockerClient().killContainerCmd(containerId)) {
            kill.withSignal(signal).exec();
        } catch (ConflictException | NotFoundException e) {
            LOG.debug("Container {} is not running: {}", containerId, e.getMessage());
        }
    }

    /**
     * Runs a command in a running container and waits for it.
     *
     * @param containerId the container
     * @param timeoutSeconds the timeout in seconds
     * @param output receives the output of the command, line by line
     * @param command the command and its arguments
     * @return the exit code of the command
     * @throws IOException if the command does not finish within the timeout
     * @since 5.1.0
     */
    public int execInContainer(String containerId, int timeoutSeconds, Consumer<String> output, String... command) throws IOException {
        String execId;
        try (ExecCreateCmd create = getDockerClient().execCreateCmd(containerId)) {
            execId = create.withCmd(command).withAttachStdout(true).withAttachStderr(true).exec().getId();
        }
        try (var callback = new LineCallback(output)) {
            boolean completed = getDockerClient().execStartCmd(execId).exec(callback).awaitCompletion(timeoutSeconds, TimeUnit.SECONDS);
            callback.flush();
            if (!completed) {
                throw new IOException("Command " + command[0] + " did not finish within " + timeoutSeconds + " seconds");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while running " + command[0], e);
        }
        Long exitCode = getDockerClient().inspectExecCmd(execId).exec().getExitCodeLong();
        return exitCode == null ? -1 : exitCode.intValue();
    }

    /**
     * Runs an image with the given entrypoint, waits for it and returns its output.
     *
     * @param imageId the image to use
     * @param timeoutSeconds the timeout in seconds
     * @param entrypoint the entrypoint, which replaces the image entrypoint and command
     * @return the exit code and the output of the container
     * @throws IOException if the container does not exit within the timeout
     * @since 5.1.0
     */
    public ContainerOutput runAndCaptureOutput(String imageId, int timeoutSeconds, List<String> entrypoint) throws IOException {
        verifyDockerRunning();
        String containerId;
        try (CreateContainerCmd create = getDockerClient().createContainerCmd(imageId)) {
            containerId = create.withEntrypoint(entrypoint).exec().getId();
        }
        try {
            startContainer(containerId);
            int exitCode = awaitExit(containerId, timeoutSeconds);
            var lines = new ArrayList<String>();
            try (var callback = new LineCallback(lines::add)) {
                getDockerClient().logContainerCmd(containerId).withStdOut(true).withStdErr(true).exec(callback).awaitCompletion();
                callback.flush();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            return new ContainerOutput(exitCode, String.join("\n", lines));
        } finally {
            removeContainer(containerId);
        }
    }

    /**
     * Logs the output of a container.
     *
     * @param containerId the container
     * @since 5.1.0
     */
    public void logContainerOutput(String containerId) {
        final Slf4jLogConsumer stdoutConsumer = new Slf4jLogConsumer(LOG);
        final Slf4jLogConsumer stderrConsumer = new Slf4jLogConsumer(LOG);

        try (var callback = new FrameConsumerResultCallback()) {
            callback.addConsumer(OutputFrame.OutputType.STDOUT, stdoutConsumer);
            callback.addConsumer(OutputFrame.OutputType.STDERR, stderrConsumer);

            getDockerClient().logContainerCmd(containerId)
                .withStdOut(true)
                .withStdErr(true)
                .exec(callback)
                .awaitCompletion();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (IOException e) {
            LOG.warn("Could not read the output of container {}: {}", containerId, e.getMessage());
        }
    }

    /**
     * Copies a regular file out of a container, which may have exited.
     *
     * @param containerId the container
     * @param containerPath the absolute path of the file in the container
     * @param target the file to write
     * @throws IOException if the file cannot be copied
     * @since 5.1.0
     */
    public void copyFileFromContainer(String containerId, String containerPath, Path target) throws IOException {
        try (InputStream archive = getDockerClient().copyArchiveFromContainerCmd(containerId, containerPath).exec();
             var tar = new TarArchiveInputStream(archive)) {
            TarArchiveEntry entry = tar.getNextEntry();
            if (entry == null || !entry.isFile()) {
                throw new IOException(containerPath + " is not a file in container " + containerId);
            }
            Files.createDirectories(target.toAbsolutePath().getParent());
            Files.copy(tar, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (NotFoundException e) {
            throw new IOException(containerPath + " does not exist in container " + containerId, e);
        }
    }

    /**
     * Removes a container, stopping it if it is running.
     *
     * @param containerId the container
     * @since 5.1.0
     */
    public void removeContainer(String containerId) {
        try (RemoveContainerCmd remove = getDockerClient().removeContainerCmd(containerId)) {
            remove.withForce(true).withRemoveVolumes(true).exec();
        } catch (NotFoundException e) {
            LOG.debug("Container {} was already removed", containerId);
        }
    }

    /**
     * Removes an image and its tags.
     *
     * @param imageId the image
     * @since 5.1.0
     */
    public void removeImage(String imageId) {
        try (RemoveImageCmd remove = getDockerClient().removeImageCmd(imageId)) {
            remove.withForce(true).exec();
        } catch (NotFoundException e) {
            LOG.debug("Image {} was already removed", imageId);
        }
    }

    /**
     * @param image the image name or ID
     * @return the image details
     * @since 5.1.0
     */
    public InspectImageResponse inspectImage(String image) {
        verifyDockerRunning();
        try (InspectImageCmd inspect = getDockerClient().inspectImageCmd(image)) {
            return inspect.exec();
        }
    }

    /**
     * @return the platform of the Docker daemon, as {@code os/architecture} with Go architecture names such as
     * {@code linux/amd64} or {@code linux/arm64}
     * @throws IllegalStateException if the Docker daemon is not reachable
     * @since 5.1.0
     */
    public String getDaemonPlatform() {
        verifyDockerRunning();
        Info info = getDockerClient().infoCmd().exec();
        return info.getOsType() + "/" + goArchitecture(info.getArchitecture());
    }

    /**
     * @param architecture an architecture name, as the Docker daemon or Jib configuration gives it
     * @return the Go architecture name used in image platforms, such as {@code amd64} or {@code arm64}
     * @since 5.1.0
     */
    public static String goArchitecture(String architecture) {
        return switch (architecture) {
            case "x86_64", "amd64" -> "amd64";
            case "aarch64", "arm64" -> "arm64";
            default -> architecture;
        };
    }

    /**
     * Copies a file from the specified container path in the given image ID, into a temporal location.
     *
     * @param imageId The image ID.
     * @param containerPath The container path.
     * @return The temporal file.
     */
    public File copyFromContainer(String imageId, String containerPath) {
        CreateContainerCmd containerCmd = getDockerClient().createContainerCmd(imageId);
        CreateContainerResponse container = containerCmd.exec();
        getDockerClient().startContainerCmd(container.getId());
        InputStream nativeImage = getDockerClient().copyArchiveFromContainerCmd(container.getId(), containerPath).exec();

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
        return getDockerClient().pushImageCmd(imageName);
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
        boolean loginSucceeded = false;
        try {
            AuthResponse authResponse = getDockerClient().authCmd().withAuthConfig(authConfig).exec();
            if (authResponse.getStatus() != null && authResponse.getStatus().equals("Login Succeeded")) {
                loginSucceeded = true;
            }
        } catch (Exception ignored) {
            // typically this is com.github.dockerjava.api.exception.UnauthorizedException
        }

        if (loginSucceeded) {
            LOG.info("Successfully logged in to registry {}", dockerImageName.getRegistry());
        } else {
            LOG.warn("Failed to login to registry {}", dockerImageName.getRegistry());
        }
        return authConfig;
    }

    private void verifyDockerRunning() {
        try {
            getDockerClient().pingCmd().exec();
        } catch (DockerException e) {
            throw new IllegalStateException(e.getMessage());
        } catch (RuntimeException e) {
            throw new IllegalStateException("Cannot connect to the Docker daemon at " + config.getDockerHost() + ". Is the docker daemon running?", e);
        }
    }

    /**
     * The exit code and the output of a container.
     *
     * @param exitCode the exit code
     * @param output the standard output and error, interleaved
     * @since 5.1.0
     */
    public record ContainerOutput(int exitCode, String output) {
    }

    /**
     * Splits the frames of a container stream into lines.
     */
    private static final class LineCallback extends ResultCallback.Adapter<Frame> {
        private final Consumer<String> lines;
        private final StringBuilder pending = new StringBuilder();

        private LineCallback(Consumer<String> lines) {
            this.lines = lines;
        }

        @Override
        public void onNext(Frame frame) {
            pending.append(new String(frame.getPayload(), StandardCharsets.UTF_8));
            int newLine = pending.indexOf("\n");
            while (newLine >= 0) {
                lines.accept(StringUtils.removeEnd(pending.substring(0, newLine), "\r"));
                pending.delete(0, newLine + 1);
                newLine = pending.indexOf("\n");
            }
        }

        private void flush() {
            if (!pending.isEmpty()) {
                lines.accept(pending.toString());
                pending.setLength(0);
            }
        }
    }
}
