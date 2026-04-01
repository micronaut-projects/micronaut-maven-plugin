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

import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.jib.JibMicronautExtension;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.graalvm.buildtools.utils.NativeImageUtils;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.maven.DockerNativeMojo.ARGS_FILE_PROPERTY_NAME;

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
@Execute(phase = LifecyclePhase.PROCESS_CLASSES)
public class DockerfileMojo extends AbstractDockerMojo {

    public static final String DOCKERFILE = "Dockerfile";
    public static final String DOCKERFILE_AWS_CUSTOM_RUNTIME = "DockerfileNativeLambda";
    public static final String DOCKERFILE_AWS = "DockerfileLambda";
    public static final String DOCKERFILE_ORACLE_CLOUD = "DockerfileOracleCloud";
    public static final String DOCKERFILE_NATIVE = "DockerfileNative";
    public static final String DOCKERFILE_CRAC = "DockerfileCrac";
    public static final String DOCKERFILE_CRAC_CHECKPOINT = "DockerfileCracCheckpoint";
    public static final String DOCKERFILE_CRAC_CHECKPOINT_FILE = "Dockerfile.crac.checkpoint";
    public static final String DOCKERFILE_NATIVE_DISTROLESS = "DockerfileNativeDistroless";
    public static final String DOCKERFILE_NATIVE_STATIC = "DockerfileNativeStatic";
    public static final String DOCKERFILE_NATIVE_ORACLE_CLOUD = "DockerfileNativeOracleCloud";
    public static final String NATIVE_BUILD_TOOLS_MAVEN_PLUGIN = "org.graalvm.buildtools:native-maven-plugin";

    private final ExecutorService executorService;

    @Inject
    public DockerfileMojo(MavenProject mavenProject, DockerService dockerService, JibConfigurationService jibConfigurationService,
                          ApplicationConfigurationService applicationConfigurationService, ExecutorService executorService,
                          MavenSession mavenSession, MojoExecution mojoExecution) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession, mojoExecution);
        this.executorService = executorService;
    }

    @Override
    public void execute() throws MojoExecutionException {
        var runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());
        var packaging = Packaging.of(mavenProject.getPackaging());
        try {
            copyDependencies();
            var dockerfile = switch (packaging) {
                case DOCKER_NATIVE -> buildDockerfileNative(runtime);
                case DOCKER -> buildDockerfile(runtime);
                case DOCKER_CRAC -> buildCracDockerfile(runtime);
                default -> throw new MojoExecutionException("Packaging is set to [" + packaging + "]. To generate a Dockerfile, set the packaging to either [" + Packaging.DOCKER.id() + "] or [" + Packaging.DOCKER_NATIVE.id() + "]");
            };

            dockerfile.ifPresent(file -> getLog().info("Dockerfile written to: " + file.getAbsolutePath()));

        } catch (IOException | MavenInvocationException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private Optional<File> buildDockerfile(MicronautRuntime runtime) throws IOException, MojoExecutionException {
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_ORACLE_CLOUD);
                oracleCloudFunctionCmd(dockerfile);
                processOracleFunctionDockerfile(dockerfile);
            }
            case LAMBDA -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_AWS);
                processDockerfile(dockerfile);
            }
            case DEFAULT -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE);
                processDockerfile(dockerfile);
            }
            default -> throw new IllegalStateException("Unexpected value: " + runtime.getBuildStrategy());
        }
        return Optional.ofNullable(dockerfile);
    }

    private Optional<File> buildCracDockerfile(MicronautRuntime runtime) throws IOException, MojoExecutionException {
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION -> throw new MojoExecutionException("Oracle Functions are currently unsupported");
            case LAMBDA -> throw new MojoExecutionException("Lambda Functions are currently unsupported");
            case DEFAULT -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_CRAC_CHECKPOINT, DOCKERFILE_CRAC_CHECKPOINT_FILE);
                processDockerfile(dockerfile);
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_CRAC);
                processDockerfile(dockerfile);
            }
            default -> throw new IllegalStateException("Unexpected value: " + runtime.getBuildStrategy());
        }
        return Optional.ofNullable(dockerfile);
    }

    static void processOracleFunctionDockerfile(File dockerfile) throws IOException {
        if (dockerfile != null) {
            var allLines = Files.readAllLines(dockerfile.toPath());
            String projectFnVersion = JibMicronautExtension.determineProjectFnVersion(System.getProperty("java.version"));
            allLines.add(0, allLines.remove(0) + projectFnVersion);
            String entrypoint = JibMicronautExtension.buildProjectFnEntrypoint()
                .stream()
                .map(s -> "\"" + s + "\"")
                .collect(Collectors.joining(", "));

            allLines.add("ENTRYPOINT [" + entrypoint + "]");

            Files.write(dockerfile.toPath(), allLines);
        }
    }

    private Optional<File> buildDockerfileNative(MicronautRuntime runtime) throws IOException, MavenInvocationException, MojoExecutionException {
        getLog().info("Generating GraalVM args file");
        executorService.invokeGoal(NATIVE_BUILD_TOOLS_MAVEN_PLUGIN, "write-args-file");
        File dockerfile;
        switch (runtime.getBuildStrategy()) {
            case LAMBDA -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_AWS_CUSTOM_RUNTIME);
                lambdaBootstrapCommand(dockerfile);
            }
            case ORACLE_FUNCTION -> {
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_NATIVE_ORACLE_CLOUD);
                oracleCloudFunctionCmd(dockerfile);
            }
            case DEFAULT -> {
                String dockerfileName = DOCKERFILE_NATIVE;
                if (Boolean.TRUE.equals(staticNativeImage)) {
                    getLog().info("Generating a static native image");
                    dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_STATIC;
                } else if (baseImageRun.contains("distroless")) {
                    getLog().info("Generating a mostly static native image");
                    dockerfileName = DockerfileMojo.DOCKERFILE_NATIVE_DISTROLESS;
                }
                dockerfile = dockerService.loadDockerfileAsResource(dockerfileName);
            }
            default -> throw new IllegalStateException("Unexpected value: " + runtime.getBuildStrategy());
        }
        processDockerfile(dockerfile);
        return Optional.ofNullable(dockerfile);
    }

    private void processDockerfile(File dockerfile) throws IOException, MojoExecutionException {
        if (dockerfile == null) {
            return;
        }

        var allLines = Files.readAllLines(dockerfile.toPath());
        Files.write(dockerfile.toPath(), processDockerfileLines(allLines));

        String argsFile = findArgsFile();
        if (argsFile != null) {
            processNativeImageArgs(argsFile);
        }
    }

    private List<String> processDockerfileLines(List<String> allLines) throws MojoExecutionException {
        var result = new ArrayList<String>();
        for (String line : allLines) {
            String processedLine = processDockerfileLine(line);
            if (processedLine != null) {
                result.add(processedLine);
            }
        }
        return result;
    }

    private String processDockerfileLine(String line) throws MojoExecutionException {
        if (line.startsWith("ARG")) {
            return shouldInlineArgLine(line) ? null : line;
        }
        if (line.contains("BASE_IMAGE_RUN")) {
            return line.replace("${BASE_IMAGE_RUN}", validateImageReference("micronaut.native-image.base-image-run", baseImageRun));
        }
        if (line.contains("BASE_IMAGE")) {
            return line.replace("${BASE_IMAGE}", validateImageReference("jib.from.image", getFrom()));
        }
        if (line.contains("BASE_JAVA_IMAGE")) {
            return line.replace("${BASE_JAVA_IMAGE}", validateImageReference("base Java image", getBaseImage()));
        }
        if (line.contains("GRAALVM_DOWNLOAD_URL")) {
            return line.replace("${GRAALVM_DOWNLOAD_URL}", shellLiteral("GraalVM download URL", validateDownloadUrl("GraalVM download URL", graalVmDownloadUrl())));
        }
        if (line.contains("CLASS_NAME")) {
            return replaceClassName(line);
        }
        if (line.contains("PORTS")) {
            return line.replace("${PORTS}", validateExposedPorts("jib.container.ports", getPorts()));
        }
        return line;
    }

    private static boolean shouldInlineArgLine(String line) {
        return line.contains("BASE_IMAGE_RUN")
            || line.contains("BASE_JAVA_IMAGE")
            || line.contains("BASE_IMAGE")
            || line.contains("GRAALVM_DOWNLOAD_URL")
            || line.contains("CLASS_NAME")
            || line.contains("PORTS");
    }

    private String replaceClassName(String line) throws MojoExecutionException {
        String className = line.contains("ENTRYPOINT [")
            ? escapeJsonString("exec.mainClass", mainClass)
            : line.contains("\"${CLASS_NAME}\"")
                ? escapeShellDoubleQuoted("exec.mainClass", mainClass)
            : shellLiteral("exec.mainClass", mainClass);
        return line.replace("${CLASS_NAME}", className);
    }

    private static String escapeShellDoubleQuoted(String source, String value) throws MojoExecutionException {
        return validateDockerfileValue(source, value)
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("$", "\\$")
            .replace("`", "\\`");
    }

    private String findArgsFile() throws IOException {
        String argsFile = mavenProject.getProperties().getProperty(ARGS_FILE_PROPERTY_NAME);
        if (argsFile != null) {
            return argsFile;
        }
        Path targetPath = Paths.get(mavenProject.getBuild().getDirectory());
        try (Stream<Path> listStream = Files.list(targetPath)) {
            return listStream
                .filter(path -> {
                    String fileName = path.getFileName().toString();
                    return fileName.startsWith("native-image") && fileName.endsWith("args");
                })
                .findFirst()
                .map(path -> path.toAbsolutePath().toString())
                .orElse(null);
        }
    }

    private void processNativeImageArgs(String argsFile) throws IOException, MojoExecutionException {
        List<String> allNativeImageBuildArgs = MojoUtils.computeNativeImageArgs(nativeImageBuildArgs, baseImageRun, argsFile);
        //Remove extra main class argument
        allNativeImageBuildArgs.remove(mainClass);
        getLog().info("GraalVM native image build args: " + allNativeImageBuildArgs);
        List<String> conversionResult = NativeImageUtils.convertToArgsFile(allNativeImageBuildArgs, Paths.get(mavenProject.getBuild().getDirectory()));
        if (conversionResult.size() == 1) {
            Files.delete(Paths.get(argsFile));
        }
    }
}
