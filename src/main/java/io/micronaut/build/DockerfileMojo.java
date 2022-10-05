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
package io.micronaut.build;

import io.micronaut.build.jib.JibMicronautExtension;
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
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * <p>Generates a <code>Dockerfile</code> depending on the <code>packaging</code> and <code>micronaut.runtime</code>
 * properties.
 *
 * <pre>mvn mn:dockerfile -Dpackaging=docker-native -Dmicronaut.runtime=lambda</pre>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = "dockerfile", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerfileMojo extends AbstractDockerMojo {

    public static final String DOCKERFILE = "Dockerfile";
    public static final String DOCKERFILE_AWS_CUSTOM_RUNTIME = "DockerfileAwsCustomRuntime";
    public static final String DOCKERFILE_NATIVE = "DockerfileNative";
    public static final String DOCKERFILE_CRAC = "DockerfileCrac";
    public static final String DOCKERFILE_CRAC_CHECKPOINT = "DockerfileCracCheckpoint";
    public static final String DOCKERFILE_CRAC_CHECKPOINT_FILE = "Dockerfile.crac.checkpoint";
    public static final String DOCKERFILE_NATIVE_DISTROLESS = "DockerfileNativeDistroless";
    public static final String DOCKERFILE_NATIVE_STATIC = "DockerfileNativeStatic";
    public static final String DOCKERFILE_NATIVE_ORACLE_CLOUD = "DockerfileNativeOracleCloud";

    @Inject
    public DockerfileMojo(MavenProject mavenProject, DockerService dockerService, JibConfigurationService jibConfigurationService,
                          ApplicationConfigurationService applicationConfigurationService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
    }

    @Override
    public void execute() throws MojoExecutionException {
        MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());
        Packaging packaging = Packaging.of(mavenProject.getPackaging());
        try {
            copyDependencies();
            Optional<File> dockerfile;

            switch (packaging) {
                case DOCKER_NATIVE:
                    dockerfile = buildDockerfileNative(runtime);
                    break;

                case DOCKER:
                    dockerfile = buildDockerfile(runtime);
                    break;

                case DOCKER_CRAC:
                    dockerfile = buildCracDockerfile(runtime);
                    break;

                default:
                    throw new MojoExecutionException("Packaging is set to [" + packaging + "]. To generate a Dockerfile, set the packaging to either [" + Packaging.DOCKER.id() + "] or [" + Packaging.DOCKER_NATIVE.id() + "]");
            }

            dockerfile.ifPresent(file -> getLog().info("Dockerfile written to: " + file.getAbsolutePath()));

        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private Optional<File> buildDockerfile(MicronautRuntime runtime) throws IOException {
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                dockerfile = dockerService.loadDockerfileAsResource("DockerfileOracleCloud");
                processOracleFunctionDockerfile(dockerfile);
                break;
            case LAMBDA:
            case DEFAULT:
            default:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE);
                processDockerfile(dockerfile);
                break;
        }
        return Optional.ofNullable(dockerfile);
    }

    private Optional<File> buildCracDockerfile(MicronautRuntime runtime) throws IOException, MojoExecutionException {
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                throw new MojoExecutionException("Oracle Functions are currently unsupported");
            case LAMBDA:
                throw new MojoExecutionException("Lambda Functions are currently unsupported");
            case DEFAULT:
            default:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_CRAC_CHECKPOINT, DOCKERFILE_CRAC_CHECKPOINT_FILE);
                processDockerfile(dockerfile);
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_CRAC);
                processDockerfile(dockerfile);
                break;
        }
        return Optional.ofNullable(dockerfile);
    }

    private void processOracleFunctionDockerfile(File dockerfile) throws IOException {
        if (dockerfile != null) {
            List<String> allLines = Files.readAllLines(dockerfile.toPath());
            allLines.add(0, allLines.remove(0) + JibMicronautExtension.determineProjectFnVersion());
            String entrypoint = JibMicronautExtension.buildProjectFnEntrypoint()
                    .stream()
                    .map(s -> "\"" + s + "\"")
                    .collect(Collectors.joining(", "));

            allLines.add("ENTRYPOINT [" + entrypoint + "]");

            Files.write(dockerfile.toPath(), allLines);
        }
    }

    private Optional<File> buildDockerfileNative(MicronautRuntime runtime) throws IOException {
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case LAMBDA:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_AWS_CUSTOM_RUNTIME);
                break;

            case ORACLE_FUNCTION:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_NATIVE_ORACLE_CLOUD);
                break;

            case DEFAULT:
            default:
                String dockerfileName = DOCKERFILE_NATIVE;
                if (staticNativeImage) {
                    getLog().info("Generating a static native image");
                    dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_STATIC;
                } else if (baseImageRun.contains("distroless")) {
                    getLog().info("Generating a mostly static native image");
                    dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_DISTROLESS;
                }
                dockerfile = dockerService.loadDockerfileAsResource(dockerfileName);
                break;
        }
        processDockerfile(dockerfile);
        return Optional.ofNullable(dockerfile);

    }

    private void processDockerfile(File dockerfile) throws IOException {
        if (dockerfile != null) {
            List<String> allLines = Files.readAllLines(dockerfile.toPath());
            List<String> result = new ArrayList<>();

            for (String line : allLines) {
                if (!line.startsWith("ARG")) {
                    if (line.contains("BASE_IMAGE_RUN")) {
                        result.add(line.replace("${BASE_IMAGE_RUN}", baseImageRun));
                    } else if (line.contains("BASE_IMAGE")) {
                        result.add(line.replace("${BASE_IMAGE}", getFrom()));
                    } else if (line.contains("EXTRA_CMD")) {
                        if (baseImageRun.contains("alpine-glibc")) {
                            result.add("RUN apk update && apk add libstdc++");
                        } else {
                            result.add("");
                        }
                    } else if (line.contains("GRAALVM_") || line.contains("CLASS_NAME")) {
                        String graalVmBuildArgs = getGraalVmBuildArgs();
                        if (baseImageRun.contains("distroless") && !graalVmBuildArgs.contains(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG)) {
                            graalVmBuildArgs = MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG + " " + graalVmBuildArgs;
                        }

                        result.add(line
                                .replace("${GRAALVM_VERSION}", graalVmVersion())
                                .replace("${GRAALVM_JVM_VERSION}", graalVmJvmVersion())
                                .replace("${GRAALVM_ARCH}", graalVmArch())
                                .replace("${GRAALVM_ARGS}", graalVmBuildArgs)
                                .replace("${CLASS_NAME}", mainClass)
                        );
                    } else if (line.contains("PORT")) {
                        result.add(line.replace("${PORT}", getPort()));
                    } else {
                        result.add(line);
                    }
                }
            }

            if (appArguments != null && !appArguments.isEmpty()) {
                getLog().info("Using application arguments: " + appArguments);
                result.add(getCmd());
            }

            Files.write(dockerfile.toPath(), result);
        }
    }
}
