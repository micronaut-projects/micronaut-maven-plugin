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
import com.github.dockerjava.api.exception.DockerClientException;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.common.io.FileWriteMode;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.core.util.StringUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.graalvm.buildtools.utils.NativeImageUtils;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

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
    public static final String MICRONAUT_PARENT = "io.micronaut.platform:micronaut-parent";
    public static final String MICRONAUT_VERSION = "micronaut.version";
    public static final String ARGS_FILE_PROPERTY_NAME = "graalvm.native-image.args-file";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                            ApplicationConfigurationService applicationConfigurationService, DockerService dockerService,
                            MavenSession mavenSession, MojoExecution mojoExecution) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession, mojoExecution);
    }

    @Override
    public void execute() throws MojoExecutionException {
        checkJavaVersion();
        checkGraalVm();

        try {
            copyDependencies();

            MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());

            switch (runtime.getBuildStrategy()) {
                case LAMBDA -> buildDockerNativeLambda();
                case ORACLE_FUNCTION -> buildOracleCloud();
                case DEFAULT -> buildDockerNative();
                default -> throw new IllegalStateException("Unexpected value: " + runtime.getBuildStrategy());
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
                    if (!mavenProject.getInjectedProfileIds().get(MICRONAUT_PARENT + ":" + micronautParentVersion).contains("graalvm")) {
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
        Map<String, String> buildImageCmdArguments = new HashMap<>();

        getLog().info("Using GRAALVM_JVM_VERSION: " + graalVmJvmVersion());
        getLog().info("Using GRAALVM_ARCH: " + graalVmArch());

        // Starter sets the right class in pom.xml:
        //   - For applications: io.micronaut.function.aws.runtime.MicronautLambdaRuntime
        //   - For function apps: com.example.BookLambdaRuntime
        getLog().info("Using CLASS_NAME: " + mainClass);
        BuildImageCmd buildImageCmd = addNativeImageBuildArgs(buildImageCmdArguments, () -> {
            try {
                return dockerService.buildImageCmd(DockerfileMojo.DOCKERFILE_AWS_CUSTOM_RUNTIME)
                        .withBuildArg("GRAALVM_VERSION", graalVmVersion())
                        .withBuildArg("GRAALVM_JVM_VERSION", graalVmJvmVersion())
                        .withBuildArg("GRAALVM_ARCH", graalVmArch());
            } catch (IOException e) {
                throw new DockerClientException(e.getMessage(), e);
            }
        });
        buildImageCmd.withBuildArg("CLASS_NAME", mainClass);
        String imageId = dockerService.buildImage(buildImageCmd);
        File functionZip = dockerService.copyFromContainer(imageId, "/function/function.zip");
        getLog().info("AWS Lambda Custom Runtime ZIP: " + functionZip.getPath());
    }

    private void buildDockerNative() throws IOException, InvalidImageReferenceException {
        String dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE;
        if (Boolean.TRUE.equals(staticNativeImage)) {
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
            com.google.common.io.Files.asCharSink(dockerfile, Charset.defaultCharset(), FileWriteMode.APPEND).write(System.lineSeparator() + getCmd());
        }

        Map<String, String> buildImageCmdArguments = new HashMap<>();

        getLog().info("Using BASE_IMAGE: " + from);
        if (StringUtils.isNotEmpty(baseImageRun) && Boolean.FALSE.equals(staticNativeImage)) {
            buildImageCmdArguments.put("BASE_IMAGE_RUN", baseImageRun);
        }

        if (baseImageRun.contains("alpine-glibc")) {
            buildImageCmdArguments.put("EXTRA_CMD", "apk update && apk add libstdc++");
        }

        if (passClassName) {
            buildImageCmdArguments.put("CLASS_NAME", mainClass);
        }

        BuildImageCmd buildImageCmd = addNativeImageBuildArgs(buildImageCmdArguments, () -> dockerService.buildImageCmd()
                .withDockerfile(dockerfile)
                .withTags(getTags())
                .withBuildArg("BASE_IMAGE", from)
                .withBuildArg("PORT", port));

        dockerService.buildImage(buildImageCmd);
    }

    private BuildImageCmd addNativeImageBuildArgs(Map<String, String> buildImageCmdArguments, Supplier<BuildImageCmd> buildImageCmdSupplier) throws IOException {
        String argsFile = mavenProject.getProperties().getProperty(ARGS_FILE_PROPERTY_NAME);
        List<String> allNativeImageBuildArgs = MojoUtils.computeNativeImageArgs(nativeImageBuildArgs, baseImageRun, argsFile);
        //Remove extra main class argument
        allNativeImageBuildArgs.remove(mainClass);
        getLog().info("GraalVM native image build args: " + allNativeImageBuildArgs);
        List<String> conversionResult = NativeImageUtils.convertToArgsFile(allNativeImageBuildArgs, Paths.get(mavenProject.getBuild().getDirectory()));
        if (conversionResult.size() == 1) {
            Files.delete(Paths.get(argsFile));

            BuildImageCmd buildImageCmd = buildImageCmdSupplier.get();

            for (Map.Entry<String, String> buildArg : buildImageCmdArguments.entrySet()) {
                String key = buildArg.getKey();
                String value = buildArg.getValue();
                getLog().info("Using " + key + ": " + value);
                buildImageCmd.withBuildArg(key, value);
            }

            getNetworkMode().ifPresent(buildImageCmd::withNetworkMode);
            return buildImageCmd;
        } else {
            throw new IOException("Unable to convert native image build args to args file");
        }
    }

}
