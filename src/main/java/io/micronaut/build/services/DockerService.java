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
package io.micronaut.build.services;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.*;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import io.micronaut.build.DockerfileMojo;
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
            String canonicalDestinationPath = file.getCanonicalPath();
            if (!canonicalDestinationPath.startsWith(mavenProject.getBuild().getDirectory())) {
                throw new IOException("Entry is outside of the target directory");
            }

            IOUtils.copy(fin, Files.newOutputStream(file.toPath()));

            return file;
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            containerCmd.close();
        }
        return null;
    }

    /**
     * Loads the given Dockerfile as classpath resource and copies it into a temporary location in the target directory.
     */
    public File loadDockerfileAsResource(String name) throws IOException {
        String path = "/dockerfiles/" + name;
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) {
            File dockerfile = new File(mavenProject.getBuild().getDirectory(), DockerfileMojo.DOCKERFILE);
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
