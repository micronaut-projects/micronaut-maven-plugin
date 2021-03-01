package io.micronaut.build.services;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.PushImageCmd;
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
import org.apache.commons.lang.StringUtils;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Provides methods to work with Docker images
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

    public BuildImageCmd buildImageCmd(String dockerfileName) throws IOException {
        return dockerClient.buildImageCmd(loadDockerfileAsResource(dockerfileName));
    }

    public BuildImageCmd buildImageCmd() {
        return dockerClient.buildImageCmd();
    }

    public String buildImage(BuildImageCmd builder) {
        BuildImageResultCallback resultCallback = new BuildImageResultCallback() {
            @Override
            public void onNext(BuildResponseItem item) {
                super.onNext(item);
                if (item.isErrorIndicated()) {
                    LOG.error(item.getErrorDetail().getMessage());
                } else if (item.getStream() != null) {
                    LOG.info(StringUtils.chomp(item.getStream(), System.lineSeparator()));
                }
            }
        };

        return builder
                .exec(resultCallback)
                .awaitImageId();
    }

    public File copyFromContainer(String imageId, String containerPath) {
        CreateContainerResponse container = dockerClient.createContainerCmd(imageId).exec();
        dockerClient.startContainerCmd(container.getId());
        InputStream nativeImage = dockerClient.copyArchiveFromContainerCmd(container.getId(), containerPath).exec();

        try (TarArchiveInputStream fin = new TarArchiveInputStream(nativeImage)) {
            TarArchiveEntry tarEntry = fin.getNextTarEntry();
            File file = new File(mavenProject.getBuild().getDirectory(), tarEntry.getName());
            IOUtils.copy(fin, new FileOutputStream(file));

            return file;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

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

    public PushImageCmd pushImageCmd(String imageName) {
        return dockerClient.pushImageCmd(imageName);
    }

}
