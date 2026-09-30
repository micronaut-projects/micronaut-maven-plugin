package io.micronaut.maven;

import io.micronaut.maven.jdkaotcache.MicronautJars;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DockerfileMojoTest {

    private static final String DEFAULT_START_MESSAGE = "JDK AOT cache: training mode start: the training run starts the "
        + "application, because its Micronaut version has no training mode that loads it without starting it "
        + "(micronaut.application.training.mode). The services it needs at start-up must be reachable from the image "
        + "build. With a Micronaut version that has that mode, load becomes the default: set "
        + "micronaut.docker.jdkAotCache.trainingMode=start to keep starting the application";

    @Test
    void processDockerfileEscapesJsonAndShellContexts(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080 8443/tcp"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App$USER `touch /tmp/pwned` \"Quoted\" \\\\";
        mojo.baseImageRun = "cgr.dev/chainguard/wolfi-base:latest";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "FROM ${BASE_IMAGE}",
            "EXPOSE ${PORTS}",
            "ENTRYPOINT [\"java\", \"-cp\", \"/app\", \"${CLASS_NAME}\"]",
            "RUN echo ${CLASS_NAME}",
            "RUN native-image -H:Class=\"${CLASS_NAME}\""
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        var jsonClassName = AbstractDockerMojo.escapeJsonString("exec.mainClass", mojo.mainClass);
        var shellLiteralClassName = AbstractDockerMojo.shellLiteral("exec.mainClass", mojo.mainClass);
        var doubleQuotedClassName = mojo.mainClass
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("$", "\\$")
            .replace("`", "\\`");

        assertEquals(
            java.util.List.of(
                "FROM ghcr.io/example/builder:1.0",
                "EXPOSE 8080 8443/tcp",
                "ENTRYPOINT [\"java\", \"-cp\", \"/app\", \"" + jsonClassName + "\"]",
                "RUN echo " + shellLiteralClassName,
                "RUN native-image -H:Class=\"" + doubleQuotedClassName + "\""
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DockerfileNative",
        "DockerfileNativeDistroless",
        "DockerfileNativeStatic",
        "DockerfileNativeLambda"
    })
    void nativeDockerfileTemplatesQuoteClassNameExpansion(String dockerfileName) throws IOException {
        var dockerfile = Path.of("src", "main", "resources", "dockerfiles", dockerfileName);
        var content = Files.readString(dockerfile);

        assertEquals(1, content.lines().filter(line -> line.contains("-H:Class=\"${CLASS_NAME}\"")).count());
        assertFalse(content.contains("-H:Class=${CLASS_NAME}"));
    }

    @Test
    void processDockerfileRejectsInjectedMainClass(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App\"\nRUN echo injected";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "ENTRYPOINT [\"java\", \"${CLASS_NAME}\"]");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("exec.mainClass contains an unsupported control character"));
    }

    @Test
    void processDockerfileRejectsInjectedMainClassInDoubleQuotedNativeImageBranch(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App\nRUN echo injected";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "RUN native-image -H:Class=\"${CLASS_NAME}\"");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("exec.mainClass contains an unsupported control character"));
    }

    @Test
    void processDockerfileTreatsCmdArrayClassNameAsJsonContext(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App$USER `quoted` \"Slash\\\\\"";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "CMD [\"${CLASS_NAME}\"]");

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            "CMD [\"" + AbstractDockerMojo.escapeJsonString("exec.mainClass", mojo.mainClass) + "\"]",
            Files.readAllLines(dockerfile).get(0)
        );
    }

    @Test
    void processDockerfileRejectsInvalidBaseImageReference(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.baseImageRun = "invalid image";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "FROM ${BASE_IMAGE_RUN}");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("micronaut.native-image.base-image-run is not a valid Docker image reference"));
    }

    @Test
    void processDockerfileRejectsInvalidPortTokens(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080 RUN"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "EXPOSE ${PORTS}");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("jib.container.ports contains an invalid exposed port token"));
    }

    @Test
    void processDockerfileQuotesDownloadUrlInRunInstruction(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        project.getProperties().setProperty("maven.compiler.release", "25");
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "RUN curl -4 -L ${GRAALVM_DOWNLOAD_URL} -o /tmp/graalvm.tar.gz");

        invokeProcessDockerfile(mojo, dockerfile);

        assertTrue(Files.readString(dockerfile).contains("-L " + AbstractDockerMojo.shellLiteral("GraalVM download URL", mojo.graalVmDownloadUrl()) + " -o"));
    }

    @Test
    void processDockerfilePreservesUnknownArgLines(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "ARG BASE_IMAGE",
            "ARG CHECKPOINT_IMAGE",
            "ARG CRAC_ARCH",
            "FROM ${BASE_IMAGE}",
            "FROM ${CHECKPOINT_IMAGE} AS crac-checkpoint",
            "RUN echo $CRAC_ARCH"
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            java.util.List.of(
                "ARG CHECKPOINT_IMAGE",
                "ARG CRAC_ARCH",
                "FROM ghcr.io/example/builder:1.0",
                "FROM ${CHECKPOINT_IMAGE} AS crac-checkpoint",
                "RUN echo $CRAC_ARCH"
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @Test
    void processDockerfilePreservesSimilarlyNamedArgLines(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "ARG BASE_IMAGE_TAG=latest",
            "ARG PORTS_FILE=/tmp/ports",
            "ARG CLASS_NAME_SUFFIX=Application",
            "ARG BASE_IMAGE",
            "FROM ${BASE_IMAGE}"
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            java.util.List.of(
                "ARG BASE_IMAGE_TAG=latest",
                "ARG PORTS_FILE=/tmp/ports",
                "ARG CLASS_NAME_SUFFIX=Application",
                "FROM ghcr.io/example/builder:1.0"
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @Test
    void processDockerfileDoesNotValidateSimilarlyNamedPlaceholders(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("invalid image"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App\nRUN echo injected";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "FROM ${BASE_IMAGE_TAG}",
            "CMD [\"${CLASS_NAME_SUFFIX}\"]"
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            java.util.List.of(
                "FROM ${BASE_IMAGE_TAG}",
                "CMD [\"${CLASS_NAME_SUFFIX}\"]"
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @Test
    void processDockerfileAddsSharedArenaSupportForDefaultGraalVm25Builder(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        project.getProperties().setProperty("maven.compiler.release", "25");
        Path argsFile = tempDir.resolve("target").resolve("native-image.args");
        Files.writeString(argsFile, "--no-fallback\n");
        project.getProperties().setProperty(DockerNativeMojo.ARGS_FILE_PROPERTY_NAME, argsFile.toString());
        var mojo = newDockerfileMojo(project, Optional.empty());
        mojo.baseImageRun = AbstractDockerMojo.DEFAULT_BASE_IMAGE_GRAALVM_RUN;

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "FROM builder");

        invokeProcessDockerfile(mojo, dockerfile);

        assertTrue(Files.readString(findConvertedArgsFile(tempDir.resolve("target"))).contains(MojoUtils.SHARED_ARENA_SUPPORT));
    }

    @Test
    void processDockerfileSkipsSharedArenaSupportForCustomGraalVm21Builder(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        Path argsFile = tempDir.resolve("target").resolve("native-image.args");
        Files.writeString(argsFile, "--no-fallback\n");
        project.getProperties().setProperty(DockerNativeMojo.ARGS_FILE_PROPERTY_NAME, argsFile.toString());
        var mojo = newDockerfileMojo(project, Optional.empty());
        mojo.baseImageRun = AbstractDockerMojo.DEFAULT_BASE_IMAGE_GRAALVM_RUN;
        mojo.baseImage = "container-registry.oracle.com/graalvm/native-image:21-ol8";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "FROM builder");

        invokeProcessDockerfile(mojo, dockerfile);

        assertFalse(Files.readString(findConvertedArgsFile(tempDir.resolve("target"))).contains(MojoUtils.SHARED_ARENA_SUPPORT));
    }

    @Test
    void jdkAotCacheGeneratesATrainingDockerfileWithAJarOnlyClassPath(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        when(project.getPackaging()).thenReturn("docker");
        Path classes = Files.createDirectories(tempDir.resolve("target/classes/example"));
        Files.writeString(classes.resolve("Application.class"), "class");
        when(project.getBuild().getOutputDirectory()).thenReturn(tempDir.resolve("target/classes").toString());
        var release = dependency(tempDir, "a-1.0.jar", Artifact.SCOPE_COMPILE, false);
        var snapshot = dependency(tempDir, "b-1.0-SNAPSHOT.jar", Artifact.SCOPE_RUNTIME, true);
        var test = dependency(tempDir, "c-1.0.jar", Artifact.SCOPE_TEST, false);
        var artifacts = new LinkedHashSet<Artifact>(List.of(snapshot, test, release));
        when(project.getArtifacts()).thenReturn(artifacts);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080 8443/tcp"));
        var dockerService = mock(DockerService.class);
        when(dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_JDK_AOT_CACHE)).thenAnswer(invocation -> {
            Path dockerfile = tempDir.resolve("target/Dockerfile");
            Files.copy(Path.of("src/main/resources/dockerfiles", DockerfileMojo.DOCKERFILE_JDK_AOT_CACHE), dockerfile);
            return dockerfile.toFile();
        });
        var mojo = new DockerfileMojo(project, dockerService, jibConfigurationService,
            mock(ApplicationConfigurationService.class), mock(ExecutorService.class), mockSession(project), mock(MojoExecution.class));
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.Application";
        mojo.jdkAotCache = true;
        mojo.jdkAotCacheTrainingPaths = List.of("/hello", "/it's");
        mojo.jdkAotCacheTrainingTimeout = 90;

        mojo.execute();

        assertEquals(List.of(
            "FROM eclipse-temurin:25-jre",
            "WORKDIR /home/app",
            "COPY dependency/release/ /home/app/libs/release/",
            "COPY dependency/snapshot/ /home/app/libs/snapshot/",
            "COPY jdk-aot-cache/application.jar jdk-aot-cache/classpath jdk-aot-cache/training.sh /home/app/",
            "RUN bash /home/app/training.sh train /home/app/app.aot 8080 90 sigterm '/hello' '/it'\"'\"'s' -- java -XX:+UseG1GC "
                + "-cp @/home/app/classpath 'example.Application' && rm /home/app/training.sh",
            "EXPOSE 8080 8443/tcp",
            "ENTRYPOINT [\"java\", \"-XX:+UseG1GC\", \"-XX:AOTCache=/home/app/app.aot\", \"-cp\", \"@/home/app/classpath\", "
                + "\"example.Application\"]"
        ), Files.readAllLines(tempDir.resolve("target/Dockerfile")));
        assertEquals("\"/home/app/libs/snapshot/b-1.0-SNAPSHOT.jar:/home/app/libs/release/a-1.0.jar:/home/app/application.jar\"\n",
            Files.readString(tempDir.resolve("target/jdk-aot-cache/classpath")));
        assertTrue(Files.size(tempDir.resolve("target/jdk-aot-cache/application.jar")) > 0);
        assertTrue(Files.exists(tempDir.resolve("target/jdk-aot-cache/training.sh")));
        assertTrue(Files.exists(tempDir.resolve("target/dependency/release/a-1.0.jar")));
    }

    @Test
    void jdkAotCacheOnlySupportsTheDefaultRuntime(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        when(project.getPackaging()).thenReturn("docker");
        when(project.getBuild().getOutputDirectory()).thenReturn(tempDir.resolve("target/classes").toString());
        var mojo = new DockerfileMojo(project, mock(DockerService.class), mock(JibConfigurationService.class),
            mock(ApplicationConfigurationService.class), mock(ExecutorService.class), mockSession(project), mock(MojoExecution.class));
        mojo.micronautRuntime = "lambda";
        mojo.jdkAotCache = true;

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("micronaut.docker.jdkAotCache only supports the default runtime, not lambda", ex.getMessage());
    }

    @Test
    void jdkAotCacheTrainsWithoutStartingTheApplicationWhenTheMicronautVersionCan(@TempDir Path tempDir) throws Exception {
        var mojo = jdkAotCacheMojo(tempDir, MicronautJars.withLoadMode(tempDir), Optional.of("8080"));
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertEquals("RUN bash /home/app/training.sh train /home/app/app.aot 8080 180 load -- java -XX:+UseG1GC "
            + "-cp @/home/app/classpath 'example.Application' && rm /home/app/training.sh", runInstruction(tempDir));
        assertTrue(infoMessages(log).contains("JDK AOT cache: training mode load, the default: the application loads its "
            + "bean definitions and exits without starting, so the training needs none of the services that its beans "
            + "use. If the application can start in the image build, micronaut.docker.jdkAotCache.trainingMode=start "
            + "trains a more complete cache"), infoMessages(log).toString());
        assertEquals(List.of(), warnMessages(log));
    }

    @Test
    void jdkAotCacheLoadModeCanBeConfiguredInAnyCase(@TempDir Path tempDir) throws Exception {
        var mojo = jdkAotCacheMojo(tempDir, MicronautJars.withLoadMode(tempDir), Optional.of("9090 9091"));
        mojo.jdkAotCacheTrainingMode = "LOAD";

        mojo.execute();

        assertTrue(runInstruction(tempDir).startsWith("RUN bash /home/app/training.sh train /home/app/app.aot 9090 180 load -- java "),
            runInstruction(tempDir));
    }

    @Test
    void jdkAotCacheStartsTheApplicationWhenStartIsConfigured(@TempDir Path tempDir) throws Exception {
        var mojo = jdkAotCacheMojo(tempDir, MicronautJars.withLoadMode(tempDir), Optional.of("8080"));
        mojo.jdkAotCacheTrainingMode = "start";
        mojo.jdkAotCacheTrainingPaths = List.of("/hello");
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertEquals("RUN bash /home/app/training.sh train /home/app/app.aot 8080 180 switch '/hello' -- java -XX:+UseG1GC "
            + "-cp @/home/app/classpath 'example.Application' && rm /home/app/training.sh", runInstruction(tempDir));
        // The mode is set, so the build does not explain a default
        assertTrue(infoMessages(log).contains("JDK AOT cache: training mode start: the training run starts the application"),
            infoMessages(log).toString());
        assertTrue(infoMessages(log).contains("JDK AOT cache: the application warms itself up and exits (Micronaut "
            + "training-run switch)"), infoMessages(log).toString());
        assertEquals(List.of(), warnMessages(log));
    }

    @Test
    void jdkAotCacheStartsTheApplicationWhenTheMicronautVersionHasNoTrainingMode(@TempDir Path tempDir) throws Exception {
        var withSwitch = jdkAotCacheMojo(tempDir.resolve("switch"), MicronautJars.withSwitch(tempDir), Optional.of("8080"));
        var withoutSwitch = jdkAotCacheMojo(tempDir.resolve("script"), MicronautJars.withoutSwitch(tempDir), Optional.of("8080"));
        var switchLog = mock(Log.class);
        withSwitch.setLog(switchLog);
        var scriptLog = mock(Log.class);
        withoutSwitch.setLog(scriptLog);

        withSwitch.execute();
        withoutSwitch.execute();

        assertTrue(runInstruction(tempDir.resolve("switch")).contains(" train /home/app/app.aot 8080 180 switch -- java "));
        assertTrue(runInstruction(tempDir.resolve("script")).contains(" train /home/app/app.aot 8080 180 sigterm -- java "));
        // Both say why the default training run starts the application, and how each run ends
        for (Log log : List.of(switchLog, scriptLog)) {
            assertTrue(infoMessages(log).contains(DEFAULT_START_MESSAGE), infoMessages(log).toString());
            // No training paths: nothing in this build fails once the Micronaut version has the load mode
            assertEquals(List.of(), warnMessages(log));
        }
        assertTrue(infoMessages(switchLog).contains("JDK AOT cache: the application warms itself up and exits (Micronaut "
            + "training-run switch)"), infoMessages(switchLog).toString());
        assertTrue(infoMessages(scriptLog).contains("JDK AOT cache: the training script warms the application up and "
            + "stops it with SIGTERM"), infoMessages(scriptLog).toString());
    }

    @Test
    void jdkAotCacheWarnsAboutTrainingPathsWithoutAModeWhenTheMicronautVersionHasNoTrainingMode(@TempDir Path tempDir) throws Exception {
        var mojo = jdkAotCacheMojo(tempDir, MicronautJars.withoutSwitch(tempDir), Optional.of("8080"));
        mojo.jdkAotCacheTrainingPaths = List.of("/hello");
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertTrue(runInstruction(tempDir).contains(" train /home/app/app.aot 8080 180 sigterm '/hello' -- java "), runInstruction(tempDir));
        assertTrue(infoMessages(log).contains(DEFAULT_START_MESSAGE), infoMessages(log).toString());
        // This is the build that fails once the application's Micronaut version has the load mode
        assertEquals(List.of("JDK AOT cache: micronaut.docker.jdkAotCache.trainingPaths is set and "
            + "micronaut.docker.jdkAotCache.trainingMode is not. This build will fail once the application uses a "
            + "Micronaut version with the load training mode: load becomes the default, and a load training run does "
            + "not start the application, so it sends no requests. Set micronaut.docker.jdkAotCache.trainingMode=start "
            + "now to keep starting the application"), warnMessages(log));
    }

    @Test
    void jdkAotCacheRejectsTrainingPathsWithTheLoadMode(@TempDir Path tempDir) throws Exception {
        var byDefault = jdkAotCacheMojo(tempDir.resolve("default"), MicronautJars.withLoadMode(tempDir), Optional.of("8080"));
        byDefault.jdkAotCacheTrainingPaths = List.of("/hello");
        var configured = jdkAotCacheMojo(tempDir.resolve("configured"), MicronautJars.withLoadMode(tempDir), Optional.of("8080"));
        configured.jdkAotCacheTrainingPaths = List.of("/hello");
        configured.jdkAotCacheTrainingMode = "load";

        var defaultFailure = assertThrows(MojoExecutionException.class, byDefault::execute);
        var configuredFailure = assertThrows(MojoExecutionException.class, configured::execute);

        assertTrue(defaultFailure.getMessage().startsWith("micronaut.docker.jdkAotCache.trainingPaths is set, but the "
            + "training mode is load, the default with this Micronaut version"), defaultFailure.getMessage());
        assertTrue(configuredFailure.getMessage().startsWith("micronaut.docker.jdkAotCache.trainingPaths cannot be used "
            + "with micronaut.docker.jdkAotCache.trainingMode=load"), configuredFailure.getMessage());
        assertFalse(Files.exists(tempDir.resolve("default/target/Dockerfile")));
    }

    @Test
    void jdkAotCacheRejectsTheLoadModeOnAMicronautVersionWithoutIt(@TempDir Path tempDir) throws Exception {
        var mojo = jdkAotCacheMojo(tempDir, MicronautJars.withoutSwitch(tempDir), Optional.of("8080"));
        mojo.jdkAotCacheTrainingMode = "load";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().startsWith("micronaut.docker.jdkAotCache.trainingMode=load needs a Micronaut version "
            + "with the load training mode"), ex.getMessage());
    }

    private static DockerfileMojo jdkAotCacheMojo(Path baseDir, List<Artifact> artifacts, Optional<String> ports) throws IOException {
        var project = mockProject(baseDir);
        when(project.getPackaging()).thenReturn("docker");
        Path classes = Files.createDirectories(baseDir.resolve("target/classes/example"));
        Files.writeString(classes.resolve("Application.class"), "class");
        when(project.getBuild().getOutputDirectory()).thenReturn(baseDir.resolve("target/classes").toString());
        var dependencies = new LinkedHashSet<>(artifacts);
        when(project.getArtifacts()).thenReturn(dependencies);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getPorts()).thenReturn(ports);
        var dockerService = mock(DockerService.class);
        when(dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_JDK_AOT_CACHE)).thenAnswer(invocation -> {
            Path dockerfile = baseDir.resolve("target/Dockerfile");
            Files.copy(Path.of("src/main/resources/dockerfiles", DockerfileMojo.DOCKERFILE_JDK_AOT_CACHE), dockerfile);
            return dockerfile.toFile();
        });
        var mojo = new DockerfileMojo(project, dockerService, jibConfigurationService,
            mock(ApplicationConfigurationService.class), mock(ExecutorService.class), mockSession(project), mock(MojoExecution.class));
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.Application";
        mojo.jdkAotCache = true;
        mojo.jdkAotCacheTrainingTimeout = 180;
        return mojo;
    }

    /**
     * @return the INFO messages of the mojo, without the colour that its log adds
     */
    private static List<String> infoMessages(Log log) {
        ArgumentCaptor<CharSequence> messages = ArgumentCaptor.forClass(CharSequence.class);
        verify(log, atLeast(0)).info(messages.capture());
        return messages.getAllValues().stream()
            .map(message -> message.toString().replaceAll("\u001B\\[[;\\d]*m", ""))
            .toList();
    }

    private static List<String> warnMessages(Log log) {
        ArgumentCaptor<CharSequence> messages = ArgumentCaptor.forClass(CharSequence.class);
        verify(log, atLeast(0)).warn(messages.capture());
        return messages.getAllValues().stream()
            .map(message -> message.toString().replaceAll("\u001B\\[[;\\d]*m", ""))
            .toList();
    }

    private static String runInstruction(Path baseDir) throws IOException {
        return Files.readAllLines(baseDir.resolve("target/Dockerfile")).stream()
            .filter(line -> line.startsWith("RUN "))
            .findFirst()
            .orElseThrow();
    }

    private static Artifact dependency(Path tempDir, String fileName, String scope, boolean snapshot) throws IOException {
        Path file = Files.writeString(tempDir.resolve(fileName), fileName);
        var artifact = mock(Artifact.class);
        when(artifact.getFile()).thenReturn(file.toFile());
        when(artifact.getScope()).thenReturn(scope);
        when(artifact.isSnapshot()).thenReturn(snapshot);
        return artifact;
    }

    private static DockerfileMojo newDockerfileMojo(MavenProject project, Optional<String> fromImage) {
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(fromImage);
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));
        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.Application";
        mojo.staticNativeImage = false;
        mojo.oracleLinuxVersion = "ol9";
        return mojo;
    }

    private static Path findConvertedArgsFile(Path targetDir) throws IOException {
        try (var paths = Files.list(targetDir)) {
            return paths
                .filter(path -> path.getFileName().toString().endsWith(".args"))
                .findFirst()
                .orElseThrow();
        }
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        var properties = new Properties();
        tempDir.resolve("target").toFile().mkdirs();
        when(project.getBuild()).thenReturn(build);
        when(project.getProperties()).thenReturn(properties);
        when(project.getArtifacts()).thenReturn(Set.<Artifact>of());
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        return project;
    }

    private static MavenSession mockSession(MavenProject project) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(new Properties());
        return session;
    }

    private static void invokeProcessDockerfile(DockerfileMojo mojo, Path dockerfile) throws IOException, MojoExecutionException {
        try {
            Method method = DockerfileMojo.class.getDeclaredMethod("processDockerfile", java.io.File.class);
            method.setAccessible(true);
            method.invoke(mojo, dockerfile.toFile());
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof MojoExecutionException mojoExecutionException) {
                throw mojoExecutionException;
            }
            if (cause instanceof IOException ioException) {
                throw ioException;
            }
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw new AssertionError(cause);
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }
}
