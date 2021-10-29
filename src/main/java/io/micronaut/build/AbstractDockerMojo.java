package io.micronaut.build;

import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.stream.Collectors;

import static io.micronaut.build.services.ApplicationConfigurationService.DEFAULT_PORT;

/**
 * Abstract base class for mojos related to Docker files and builds.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public abstract class AbstractDockerMojo extends AbstractMojo {

    public static final String DEFAULT_GRAAL_DOCKER_VERSION = "java11";
    public static final String LATEST_TAG = "latest";
    public static final String DEFAULT_BASE_IMAGE_GRAALVM_RUN = "frolvlad/alpine-glibc:alpine-3.12";
    public static final String MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG = "-H:+StaticExecutableWithDynamicLibC";

    protected final MavenProject mavenProject;
    protected final JibConfigurationService jibConfigurationService;
    protected final ApplicationConfigurationService applicationConfigurationService;
    protected final DockerService dockerService;


    /**
     * Additional arguments that will be passed to the <code>native-image</code> executable. Note that this will only
     * be used when using a packaging of type <code>docker-native</code>. For <code>native-image</code> packaging
     * you should use the
     * <a href="https://www.graalvm.org/reference-manual/native-image/NativeImageMavenPlugin/#maven-plugin-customization">
     * Native Image Maven Plugin
     * </a> configuration options.
     */
    @Parameter(property = "micronaut.native-image.args")
    protected List<String> nativeImageBuildArgs;

    /**
     * List of additional arguments that will be passed to the application.
     */
    @Parameter(property = RunMojo.MN_APP_ARGS)
    protected List<String> appArguments;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = RunMojo.EXEC_MAIN_CLASS, required = true)
    protected String mainClass;

    /**
     * Whether to produce a static native image when using <code>docker-native</code> packaging
     */
    @Parameter(defaultValue = "false", property = "micronaut.native-image.static")
    protected Boolean staticNativeImage;

    /**
     * The target runtime of the application
     */
    @Parameter(property = MicronautRuntime.PROPERTY, defaultValue = "NONE")
    protected String micronautRuntime;

    /**
     * The Docker image used to run the native image
     * @since 1.2
     */
    @Parameter(property = "micronaut.native-image.base-image-run", defaultValue = DEFAULT_BASE_IMAGE_GRAALVM_RUN)
    protected String baseImageRun;

    protected AbstractDockerMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService, ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        this.applicationConfigurationService = applicationConfigurationService;
        this.dockerService = dockerService;
    }

    protected ArtifactVersion javaVersion() {
        return new DefaultArtifactVersion(Optional.ofNullable(mavenProject.getProperties().getProperty("maven.compiler.target")).orElse(System.getProperty("java.version")));
    }

    protected String graalVmVersion() {
        return mavenProject.getProperties().getProperty("graal.version");
    }

    protected String graalVmJvmVersion() {
        String graalVmJvmVersion = "java11";
        if (javaVersion().getMajorVersion() >= 17) {
            graalVmJvmVersion = "java17";
        }
        return graalVmJvmVersion;
    }

    protected String getFrom() {
        if (staticNativeImage) {
            // For building a static native image we need a base image with tools (cc, make,...) already installed
            return jibConfigurationService.getFromImage().orElse("ghcr.io/graalvm/graalvm-ce:" + DEFAULT_GRAAL_DOCKER_VERSION + "-" + graalVmVersion());
        } else {
            return jibConfigurationService.getFromImage().orElse("ghcr.io/graalvm/native-image:" + DEFAULT_GRAAL_DOCKER_VERSION + "-" + graalVmVersion());
        }
    }

    protected Set<String> getTags() {
        Set<String> tags = new HashSet<>();
        Optional<String> toImageOptional = jibConfigurationService.getToImage();
        String imageName = mavenProject.getArtifactId();
        if (toImageOptional.isPresent()) {
            String toImage = toImageOptional.get();
            if (toImage.contains(":")) {
                tags.add(toImage);
                imageName = toImageOptional.get().split(":")[0];
            } else {
                tags.add(toImage + ":" + LATEST_TAG);
                imageName = toImage;
            }
        } else {
            tags.add(imageName + ":" + LATEST_TAG);
        }
        for (String tag : jibConfigurationService.getTags()) {
            if (LATEST_TAG.equals(tag) && tags.stream().anyMatch(t -> t.contains(LATEST_TAG))) {
                continue;
            }
            tags.add(String.format("%s:%s", imageName, tag));
        }
        return tags;
    }

    protected String getPort() {
        String port = applicationConfigurationService.getServerPort();
        return "-1".equals(port) ? DEFAULT_PORT : port;
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    protected void copyDependencies() throws IOException {
        List<String> imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        mavenProject.setArtifactFilter(artifact -> imageClasspathScopes.contains(artifact.getScope()));
        File target = new File(mavenProject.getBuild().getDirectory(), "dependency");
        if (!target.exists()) {
            target.mkdirs();
        }
        for (Artifact dependency : mavenProject.getArtifacts()) {
            Files.copy(dependency.getFile().toPath(), target.toPath().resolve(dependency.getFile().getName()), StandardCopyOption.REPLACE_EXISTING);
        }
    }

    protected String getCmd() {
        return "CMD [" +
                appArguments.stream()
                        .map(s -> "\"" + s + "\"")
                        .collect(Collectors.joining(", ")) +
                "]";
    }

    protected String getGraalVmBuildArgs() {
        if (nativeImageBuildArgs != null && !nativeImageBuildArgs.isEmpty()) {
            return String.join(" ", nativeImageBuildArgs);
        } else {
            return "";
        }
    }

}
