package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.common.io.FileWriteMode;
import com.google.common.io.Files;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Set;

/**
 * <p>Implementation of the <code>docker-native</code> packaging.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 * using the <code>packaging</code> property, eg:</p>
 *
 * <pre>mvn package -Dpackaging=docker-native</pre>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = DockerNativeMojo.DOCKER_NATIVE_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerNativeMojo extends AbstractDockerMojo {

    public static final String DOCKER_NATIVE_PACKAGING = "docker-native";
    public static final String GRAALVM_ARGS = "GRAALVM_ARGS";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                            ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
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


        } catch (InvalidImageReferenceException iire) {
            String message = "Invalid image reference "
                    + iire.getInvalidReference()
                    + ", perhaps you should check that the reference is formatted correctly according to " +
                    "https://docs.docker.com/engine/reference/commandline/tag/#extended-description" +
                    "\nFor example, slash-separated name components cannot have uppercase letters";
            throw new MojoExecutionException(message);
        } catch (IOException | IllegalArgumentException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void checkJavaVersion() throws MojoExecutionException {
        if (javaVersion().getMajorVersion() > 11) {
            throw new MojoExecutionException("To build native images you must set the Java target byte code level to Java 11 or below");
        }
    }

    private void buildDockerNativeLambda() throws IOException {
        BuildImageCmd buildImageCmd = dockerService.buildImageCmd(DockerfileMojo.DOCKERFILE_AWS_CUSTOM_RUNTIME)
                .withBuildArg("GRAALVM_VERSION", graalVmVersion())
                .withBuildArg("GRAALVM_JVM_VERSION", graalVmJvmVersion());

        getLog().info("Using GRAALVM_VERSION: " + graalVmVersion());
        getLog().info("Using GRAALVM_JVM_VERSION: " + graalVmJvmVersion());

        String graalVmBuildArgs = getGraalVmBuildArgs();
        if (graalVmBuildArgs != null && !graalVmBuildArgs.isEmpty()) {
            getLog().info("Using GRAALVM_ARGS: " + graalVmBuildArgs);
            buildImageCmd = buildImageCmd.withBuildArg(GRAALVM_ARGS, graalVmBuildArgs);
        }

        String imageId = dockerService.buildImage(buildImageCmd);
        File functionZip = dockerService.copyFromContainer(imageId, "/function/function.zip");
        getLog().info("AWS Lambda Custom Runtime ZIP: " + functionZip.getPath());
    }

    private void buildDockerNative() throws IOException, InvalidImageReferenceException {
        String dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE;
        if (staticNativeImage) {
            getLog().info("Generating a static native image");
            dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_STATIC;
        }

        buildDockerfile(dockerfileName, true);
    }

    private void buildOracleCloud() throws IOException, InvalidImageReferenceException {
        buildDockerfile(DockerfileMojo.DOCKERFILE_NATIVE_ORACLE_CLOUD, false);
    }

    private void buildDockerfile(String dockerfileName, boolean passClassName) throws IOException, InvalidImageReferenceException {
        Set<String> tags = getTags();
        for (String tag : tags) {
            ImageReference.parse(tag);
        }

        String from = getFrom();
        String port = getPort();
        getLog().info("Exposing port: " + port);

        File dockerfile = dockerService.loadDockerfileAsResource(dockerfileName);

        if (appArguments != null && !appArguments.isEmpty()) {
            getLog().info("Using application arguments: " + appArguments);
            Files.asCharSink(dockerfile, Charset.defaultCharset(), FileWriteMode.APPEND).write(System.lineSeparator() + getCmd());
        }

        BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                .withDockerfile(dockerfile)
                .withTags(getTags())
                .withBuildArg("BASE_IMAGE", from)
                .withBuildArg("PORT", port);

        getLog().info("Using BASE_IMAGE: " + from);

        if (passClassName) {
            getLog().info("Using CLASS_NAME: " + mainClass);
            buildImageCmd = buildImageCmd.withBuildArg("CLASS_NAME", mainClass);
        }

        String graalVmBuildArgs = getGraalVmBuildArgs();
        if (graalVmBuildArgs != null && !graalVmBuildArgs.isEmpty()) {
            getLog().info("Using GRAALVM_ARGS: " + graalVmBuildArgs);
            buildImageCmd = buildImageCmd.withBuildArg(GRAALVM_ARGS, graalVmBuildArgs);
        }


        dockerService.buildImage(buildImageCmd);
    }

}
