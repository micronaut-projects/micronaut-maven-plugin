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

import static io.micronaut.build.DockerMojo.DOCKER_PACKAGING;
import static io.micronaut.build.DockerNativeMojo.DOCKER_NATIVE_PACKAGING;

/**
 * <p>Generates a <code>Dockerfile</code> depending on the <code>packaging</code> and <code>micronaut.runtime</code>
 * properties, eg:</p>
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
        String packaging = mavenProject.getPackaging();
        try {
            copyDependencies();
            Optional<File> dockerfile;

            switch (packaging) {
                case DOCKER_NATIVE_PACKAGING:
                    dockerfile = buildDockerfileNative(runtime);
                    break;

                case DOCKER_PACKAGING:
                    dockerfile = buildDockerfile(runtime);
                    break;

                default:
                    throw new MojoExecutionException("Packaging is set to [" + packaging + "]. To generate a Dockerfile, set the packaging to either [" + DOCKER_PACKAGING + "] or [" + DOCKER_NATIVE_PACKAGING + "]");
            }

            dockerfile.ifPresent(file -> getLog().info("Dockerfile written to: " + file.getAbsolutePath()));

        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private Optional<File> buildDockerfile(MicronautRuntime runtime) throws IOException {
        File dockerfile = null;
        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                dockerfile = dockerService.loadDockerfileAsResource("DockerfileOracleCloud");
                processOracleFunctionDockerfile(dockerfile);
                break;
            case LAMBDA:
            case DEFAULT:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE);
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
        File dockerfile = null;
        switch (runtime.getBuildStrategy()) {
            case LAMBDA:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_AWS_CUSTOM_RUNTIME);
                break;

            case ORACLE_FUNCTION:
                dockerfile = dockerService.loadDockerfileAsResource(DOCKERFILE_NATIVE_ORACLE_CLOUD);
                break;

            case DEFAULT:
                String dockerfileName = DOCKERFILE_NATIVE;
                if (staticNativeImage) {
                    getLog().info("Generating a static native image");
                    dockerfileName = DOCKERFILE_NATIVE_STATIC;
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
                    if (line.contains("BASE_IMAGE")) {
                        result.add(line.replace("${BASE_IMAGE}", getFrom()));
                    } else if (line.contains("GRAALVM_") || line.contains("CLASS_NAME")) {
                        result.add(line
                                .replace("${GRAALVM_VERSION}", graalVmVersion())
                                .replace("${GRAALVM_JVM_VERSION}", graalVmJvmVersion())
                                .replace("${GRAALVM_ARGS} ", getGraalVmBuildArgs())
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
