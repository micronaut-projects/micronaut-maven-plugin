package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Mojo(name = DockerNativeMojo.DOCKER_NATIVE_PACKAGING)
public class DockerNativeMojo extends AbstractDockerNativeMojo {

    public static final String DOCKER_NATIVE_PACKAGING = "docker-native";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        super(mavenProject, jibConfigurationService);
    }

    @Override
    protected String getSupportedPackaging() {
        return DOCKER_NATIVE_PACKAGING;
    }

    @Override
    protected BuildImageCmd buildImageCmd() throws IOException {
        File dockerfile = loadDockerfileAsResource("/dockerfiles/DockerfileNative");

        String from = jibConfigurationService.getFromImage().orElse(determineBaseImage());
        getLog().info("Using base image: " + from);

        Set<String> tags = new HashSet<>(Collections.singletonList(jibConfigurationService.getToImage().orElse(mavenProject.getArtifactId())));
        tags.addAll(jibConfigurationService.getTags());
        getLog().info("Using tags: " + tags);

        //TODO read GraalVM native-image plugin config to look for additional args
        return dockerClient.buildImageCmd(dockerfile)
                .withBuildArg("BASE_IMAGE", from)
                .withBuildArg("CLASS_NAME", mainClass);
    }

    private String determineBaseImage() {
        String image = "oracle/graalvm-ce:" + graalVmVersion();
        if (javaVersion().getMajorVersion() >= 11) {
            image += "-" + DEFAULT_GRAAL_JVM_VERSION;
        }
        return image;
    }

}
