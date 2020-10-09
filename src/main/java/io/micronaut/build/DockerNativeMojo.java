package io.micronaut.build;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Mojo(name = DockerNativeMojo.DOCKER_NATIVE_PACKAGING)
public class DockerNativeMojo extends AbstractMojo {

    public static final String DOCKER_NATIVE_PACKAGING = "docker-native";
    public static final String DEFAULT_GRAAL_VERSION = "20.2.0";
    public static final String DEFAULT_GRAAL_JAVA_VERSION = "java11";
    private final MavenProject mavenProject;
    private final JibConfigurationService jibConfigurationService;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}", required = true)
    private String mainClass;

    private final DockerClient dockerClient;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        DockerClientConfig config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
        DockerHttpClient httpClient = new ZerodepDockerHttpClient.Builder()
                .dockerHost(config.getDockerHost())
                .sslConfig(config.getSSLConfig())
                .build();
        dockerClient = DockerClientImpl.getInstance(config, httpClient);
    }


    @Override
    public void execute() {
        String packaging = mavenProject.getPackaging();

        if (DOCKER_NATIVE_PACKAGING.equals(packaging)) {
            try {
                copyDependencies();
                File dockerfile = loadDockerfileAsResource("/dockerfiles/DockerfileNative");

                BuildImageResultCallback resultCallback = new BuildImageResultCallback() {
                    @Override
                    public void onNext(BuildResponseItem item) {
                        super.onNext(item);
                        if (item.isErrorIndicated()) {
                            getLog().error(item.getErrorDetail().getMessage());
                        } else {
                            getLog().info(StringUtils.chomp(item.getStream(), "\n"));
                        }
                    }
                };

                Set<String> tags = new HashSet<>(Collections.singletonList(jibConfigurationService.getToImage().orElse(mavenProject.getArtifactId())));
                tags.addAll(jibConfigurationService.getTags());

                String from = jibConfigurationService.getFromImage().orElse(determineBaseImage());
                getLog().info("Using base image: " + from);

                dockerClient.buildImageCmd(dockerfile)
                        .withBuildArg("BASE_IMAGE", from)
                        .withBuildArg("CLASS_NAME", mainClass)
                        .withTags(tags)
                        .exec(resultCallback)
                        .awaitImageId();
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            getLog().warn("Skipping. POM packaging should be set to " + DOCKER_NATIVE_PACKAGING);
        }

    }

    private String determineBaseImage() {
        String graalVersion = mavenProject.getProperties().getProperty("graal.version", DEFAULT_GRAAL_VERSION);
        ArtifactVersion javaVersion = new DefaultArtifactVersion(Optional.ofNullable(mavenProject.getProperties().getProperty("maven.compiler.target")).orElse(System.getProperty("java.version")));
        String image = "oracle/graalvm-ce:" + graalVersion;
        if (javaVersion.getMajorVersion() >= 11) {
            image += "-" + DEFAULT_GRAAL_JAVA_VERSION;
        }
        return image;
    }

    private File loadDockerfileAsResource(String path) throws IOException {
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) {
            String fileName = path.substring(path.lastIndexOf("/") + 1);
            File dockerfile = new File(targetDirectory, fileName);
            FileUtils.copyInputStreamToFile(stream, dockerfile);
            return dockerfile;
        }
        return null;
    }

    private void copyDependencies() throws IOException {
        List<String> imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        mavenProject.setArtifactFilter(artifact -> imageClasspathScopes.contains(artifact.getScope()));
        File target = new File(mavenProject.getBuild().getDirectory(), "dependency");
        target.mkdirs();
        for (Artifact dependency : mavenProject.getArtifacts()) {
            Files.copy(dependency.getFile().toPath(), target.toPath().resolve(dependency.getFile().getName()), StandardCopyOption.REPLACE_EXISTING);
        }
    }
}
