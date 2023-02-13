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
package io.micronaut.maven;

import com.github.dockerjava.api.command.BuildImageCmd;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.common.io.FileWriteMode;
import com.google.common.io.Files;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.JibConfigurationService;
import io.micronaut.core.util.StringUtils;
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
 * @author Iván López
 * @since 1.1
 */
@Mojo(name = DockerNativeMojo.DOCKER_NATIVE_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerNativeMojo extends AbstractDockerMojo {

    public static final String DOCKER_NATIVE_PACKAGING = "docker-native";
    public static final String GRAALVM_ARGS = "GRAALVM_ARGS";
    public static final String MICRONAUT_PARENT = "io.micronaut:micronaut-parent";
    public static final String MICRONAUT_VERSION = "micronaut.version";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                            ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
    }

    @Override
    public void execute() throws MojoExecutionException {
        checkJavaVersion();
        checkGraalVm();

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
                default:
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

    private void checkGraalVm() throws MojoExecutionException {
        String micronautVersion = mavenProject.getProperties().getProperty(MICRONAUT_VERSION);
        if (mavenProject.hasParent()) {
            String ga = mavenProject.getParent().getGroupId() + ":" + mavenProject.getParent().getArtifactId();
            if (MICRONAUT_PARENT.equals(ga)) {
                String micronautParentVersion = mavenProject.getModel().getParent().getVersion();
                if (micronautVersion.equals(micronautParentVersion)) {
                    if (!mavenProject.getInjectedProfileIds().get("io.micronaut:micronaut-parent:" + micronautParentVersion).contains("graalvm")) {
                        String javaVendor = System.getProperty("java.vendor", "");
                        if (javaVendor.toLowerCase().contains("graalvm")) {
                            throw new MojoExecutionException("The [graalvm] profile was not activated automatically because the native-image component is not installed (or not found in your path). Either activate the profile manually (-Pgraalvm) or install the native-image component (gu install native-image), and try again");
                        } else {
                            throw new MojoExecutionException("The [graalvm] profile was not activated automatically because you are not using a GraalVM JDK. Activate the profile manually (-Pgraalvm) and try again");
                        }
                    }
                } else {
                    String message = String.format("The %s version (%s) differs from the %s property (%s). Please, make sure both refer to the same version", MICRONAUT_PARENT, micronautParentVersion, MICRONAUT_VERSION, micronautVersion);
                    throw new MojoExecutionException(message);
                }
            } else {
                getLog().warn("The parent POM of this project is not set to " + MICRONAUT_PARENT);
            }
        } else {
            getLog().warn("This project has no parent POM defined. To avoid build problems, please set the parent to " + MICRONAUT_PARENT);
        }
    }

    private void checkJavaVersion() throws MojoExecutionException {
        if (javaVersion().getMajorVersion() > 17) {
            throw new MojoExecutionException("To build native images you must set the Java target byte code level to Java 17 or below");
        }
    }

    private void buildDockerNativeLambda() throws IOException {
        BuildImageCmd buildImageCmd = dockerService.buildImageCmd(DockerfileMojo.DOCKERFILE_AWS_CUSTOM_RUNTIME)
                .withBuildArg("GRAALVM_VERSION", graalVmVersion())
                .withBuildArg("GRAALVM_JVM_VERSION", graalVmJvmVersion())
                .withBuildArg("GRAALVM_ARCH", graalVmArch());

        getLog().info("Using GRAALVM_VERSION: " + graalVmVersion());
        getLog().info("Using GRAALVM_JVM_VERSION: " + graalVmJvmVersion());
        getLog().info("Using GRAALVM_ARCH: " + graalVmArch());

        // Starter sets the right class in pom.xml:
        //   - For applications: io.micronaut.function.aws.runtime.MicronautLambdaRuntime
        //   - For function apps: com.example.BookLambdaRuntime
        getLog().info("Using CLASS_NAME: " + mainClass);
        buildImageCmd.withBuildArg("CLASS_NAME", mainClass);

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
        } else if (baseImageRun.contains("distroless")) {
            getLog().info("Generating a mostly static native image");
            dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_DISTROLESS;
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
        if (StringUtils.isNotEmpty(baseImageRun) && !staticNativeImage) {
            getLog().info("Using BASE_IMAGE_RUN: " + baseImageRun);
            buildImageCmd.withBuildArg("BASE_IMAGE_RUN", baseImageRun);
        }

        if (baseImageRun.contains("alpine-glibc")) {
            buildImageCmd.withBuildArg("EXTRA_CMD", "apk update && apk add libstdc++");
        } else {
            buildImageCmd.withBuildArg("EXTRA_CMD", "");
        }

        if (passClassName) {
            getLog().info("Using CLASS_NAME: " + mainClass);
            buildImageCmd = buildImageCmd.withBuildArg("CLASS_NAME", mainClass);
        }

        String graalVmBuildArgs = getGraalVmBuildArgs();
        if (baseImageRun.contains("distroless") && !graalVmBuildArgs.contains(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG)) {
            graalVmBuildArgs = MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG + " " + graalVmBuildArgs;
        }

        if (graalVmBuildArgs != null && !graalVmBuildArgs.isEmpty()) {
            getLog().info("Using GRAALVM_ARGS: " + graalVmBuildArgs);
            buildImageCmd = buildImageCmd.withBuildArg(GRAALVM_ARGS, graalVmBuildArgs);
        }

        dockerService.buildImage(buildImageCmd);
    }

}
