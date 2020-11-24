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

import static io.micronaut.build.DockerNativeMojo.DEFAULT_GRAAL_JVM_VERSION;

/**
 * Abstract base class for mojos related to Docker files and builds.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public abstract class AbstractDockerMojo extends AbstractMojo {

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
    @Parameter(property = "mn.appArgs")
    protected List<String> appArguments;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}", required = true)
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
        String graalVmJvmVersion = "java8";
        if (javaVersion().getMajorVersion() >= 11) {
            graalVmJvmVersion = "java11";
        }
        return graalVmJvmVersion;
    }

    protected String getFrom() {
        return jibConfigurationService.getFromImage().orElse("oracle/graalvm-ce:" + graalVmVersion() + "-" + DEFAULT_GRAAL_JVM_VERSION);
    }

    protected Set<String> getTags() {
        Set<String> tags = new HashSet<>();
        Optional<String> toImageOptional = jibConfigurationService.getToImage();
        String imageName = mavenProject.getArtifactId();
        if (toImageOptional.isPresent()) {
            tags.add(toImageOptional.get());
            imageName = toImageOptional.get().split(":")[0];
        } else {
            tags.add(imageName + ":latest");
        }
        for (String tag : jibConfigurationService.getTags()) {
            tags.add(imageName + ":" + tag);
        }
        return tags;
    }

    protected String getPort() {
        Map<String, Object> applicationConfiguration = applicationConfigurationService.getApplicationConfiguration();
        String port = applicationConfiguration.getOrDefault("MICRONAUT_SERVER_PORT", applicationConfiguration.getOrDefault("micronaut.server.port", 8080)).toString();
        return "-1".equals(port) ? "8080" : port;
    }

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
