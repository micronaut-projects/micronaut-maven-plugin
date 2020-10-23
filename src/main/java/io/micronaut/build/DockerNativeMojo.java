package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;

/**
 * <p>Implementation of the <code>docker-native</code> packaging.</p>
 * <p>
 *     <strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 *     using the <code>packaging</code> property, eg:
 *
 *    <pre><code>
 *        mvn package -Dpackaging=docker
 *    </code></pre>
 * </p>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = DockerNativeMojo.DOCKER_NATIVE_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerNativeMojo extends AbstractMojo {

    public static final String DOCKER_NATIVE_PACKAGING = "docker-native";
    public static final String DEFAULT_GRAAL_VERSION = "20.2.0";
    public static final String DEFAULT_GRAAL_JVM_VERSION = "java11";

    private final MavenProject mavenProject;
    private final JibConfigurationService jibConfigurationService;
    private final ApplicationConfigurationService applicationConfigurationService;
    private final DockerService dockerService;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}", required = true)
    private String mainClass;

    @Parameter(property = "micronaut.runtime", defaultValue = "NONE")
    private String micronautRuntime;

    @Parameter(defaultValue = "false", property = "micronaut.native-image.static")
    private Boolean staticNativeImage;

    @Parameter(property = "micronaut.native-image.args")
    private String nativeImageBuildArgs;


    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService, ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        this.applicationConfigurationService = applicationConfigurationService;
        this.dockerService = dockerService;
    }

    @Override
    public void execute() throws MojoExecutionException {
        checkJavaVersion();

        try {
            copyDependencies();

            MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());

            switch (runtime.getBuildStrategy()) {
                case LAMBDA:
                    buildDockerNativeLambda();
                    break;

                case ORACLE_FUNCTION:
                    buildOracleCloud();
                    break;

                case DEFAULT:
                    buildDockerNative();
                    break;
            }


        } catch (Exception e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void checkJavaVersion() throws MojoExecutionException {
        if (javaVersion().getMajorVersion() > 11) {
            throw new MojoExecutionException("To build native images you must set the Java target byte code level to Java 11 or below");
        }
    }

    private void buildDockerNativeLambda() throws IOException {
        String graalVmJvmVersion = "java8";
        if (javaVersion().getMajorVersion() >= 11) {
            graalVmJvmVersion = "java11";
        }


        BuildImageCmd buildImageCmd = dockerService.buildImageCmd("DockerfileAwsCustomRuntime")
                .withBuildArg("GRAALVM_VERSION", graalVmVersion())
                .withBuildArg("GRAALVM_JVM_VERSION", graalVmJvmVersion);

        getLog().info("Using GRAALVM_VERSION: " + graalVmVersion());
        getLog().info("Using GRAALVM_JVM_VERSION: " + graalVmJvmVersion);

        if (nativeImageBuildArgs != null && !nativeImageBuildArgs.trim().isEmpty()) {
            getLog().info("Using GRAALVM_ARGS: " + nativeImageBuildArgs);
            buildImageCmd = buildImageCmd.withBuildArg("GRAALVM_ARGS", nativeImageBuildArgs);
        }

        String imageId = dockerService.buildImage(buildImageCmd);
        File functionZip = dockerService.copyFromContainer(imageId, "/function/function.zip");
        getLog().info("AWS Lambda Custom Runtime ZIP: " + functionZip.getPath());
    }

    private void buildDockerNative() throws IOException {
        String dockerfileName = "DockerfileNative";
        if (staticNativeImage) {
            getLog().info("Generating a static native image");
            dockerfileName = "DockerfileNativeStatic";
        }

        buildDockerfile(dockerfileName, true);
    }

    private void buildOracleCloud() throws IOException {
        buildDockerfile("DockerfileNativeOracleCloud", false);
    }

    private void buildDockerfile(String dockerfileName, boolean passClassName) throws IOException {
        String from = jibConfigurationService.getFromImage().orElse("oracle/graalvm-ce:" + graalVmVersion() + "-" + DEFAULT_GRAAL_JVM_VERSION);

        Set<String> tags = new HashSet<>(Collections.singletonList(jibConfigurationService.getToImage().orElse(mavenProject.getArtifactId())));
        tags.addAll(jibConfigurationService.getTags());

        Map<String, Object> applicationConfiguration = applicationConfigurationService.getApplicationConfiguration();
        String port = applicationConfiguration.getOrDefault("micronaut.server.port", 8080).toString();
        getLog().info("Exposing port: " + port);

        BuildImageCmd buildImageCmd = dockerService.buildImageCmd(dockerfileName)
                .withTags(tags)
                .withBuildArg("BASE_IMAGE", from)
                .withBuildArg("PORT", port);

        getLog().info("Using BASE_IMAGE: " + from);

        if (passClassName) {
            getLog().info("Using CLASS_NAME: " + mainClass);
            buildImageCmd = buildImageCmd.withBuildArg("CLASS_NAME", mainClass);
        }

        if (nativeImageBuildArgs != null && !nativeImageBuildArgs.trim().isEmpty()) {
            getLog().info("Using GRAALVM_ARGS: " + nativeImageBuildArgs);
            buildImageCmd = buildImageCmd.withBuildArg("GRAALVM_ARGS", nativeImageBuildArgs);
        }

        dockerService.buildImage(buildImageCmd);
    }

    private String graalVmVersion() {
        return mavenProject.getProperties().getProperty("graal.version", DEFAULT_GRAAL_VERSION);
    }

    private ArtifactVersion javaVersion() {
        return new DefaultArtifactVersion(Optional.ofNullable(mavenProject.getProperties().getProperty("maven.compiler.target")).orElse(System.getProperty("java.version")));
    }

    private void copyDependencies() throws IOException {
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

}
