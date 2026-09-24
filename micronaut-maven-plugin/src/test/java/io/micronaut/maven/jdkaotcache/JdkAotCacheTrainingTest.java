package io.micronaut.maven.jdkaotcache;

import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.model.ContainerConfig;
import com.github.dockerjava.api.model.ExposedPort;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class JdkAotCacheTrainingTest {

    private static final String JAVA_25_OUTPUT = """
        [Global flags]
             ccstr AOTCache                                 =                                           {product} {default}
        openjdk version "25.0.4" 2026-07-21 LTS
        OpenJDK Runtime Environment Temurin-25.0.4+7 (build 25.0.4+7-LTS)
        """;
    private static final String JAVA_27_OUTPUT = """
        [Global flags]
             bool AOTCompatibleOopCompression              = false                          {diagnostic lp64_product} {ergonomic}
        openjdk version "27" 2026-09-15
        """;

    private final DockerService dockerService = mock(DockerService.class);
    private final Log log = mock(Log.class);

    @Test
    void validatesTrainingPaths() throws MojoExecutionException {
        assertEquals(List.of(), JdkAotCacheTraining.validateTrainingPaths(null));
        assertEquals(List.of("/hello", "/books?ids=1,2"), JdkAotCacheTraining.validateTrainingPaths(Arrays.asList(" /hello ", "", null, "/books?ids=1,2")));
        for (String invalid : List.of("hello", "/a b", "/a\nb")) {
            var e = assertThrows(MojoExecutionException.class, () -> JdkAotCacheTraining.validateTrainingPaths(List.of(invalid)));
            assertTrue(e.getMessage().startsWith("Invalid micronaut.docker.jdkAotCache.trainingPaths entry"));
        }
    }

    @Test
    void parsesTheJavaRuntime() {
        assertEquals(Optional.of(new JdkAotCacheTraining.JavaRuntime(25, false)), JdkAotCacheTraining.JavaRuntime.parse(JAVA_25_OUTPUT));
        assertEquals(Optional.of(new JdkAotCacheTraining.JavaRuntime(27, true)), JdkAotCacheTraining.JavaRuntime.parse(JAVA_27_OUTPUT));
        assertEquals(Optional.of(new JdkAotCacheTraining.JavaRuntime(8, false)), JdkAotCacheTraining.JavaRuntime.parse("openjdk version \"1.8.0_392\""));
        assertEquals(Optional.of(new JdkAotCacheTraining.JavaRuntime(25, false)), JdkAotCacheTraining.JavaRuntime.parse(
            "NOTE: Picked up JDK_JAVA_OPTIONS: -XX:+AOTCompatibleOopCompression\nopenjdk version \"25\""));
        assertEquals(Optional.empty(), JdkAotCacheTraining.JavaRuntime.parse("bash: java: command not found"));
    }

    @Test
    void trainingEnvironmentAddsTheCacheOutputToTheImageOptions() {
        var java25 = new JdkAotCacheTraining.JavaRuntime(25, false);

        assertEquals(Map.of("JDK_JAVA_OPTIONS", "-XX:AOTCacheOutput=/tmp/app.aot"),
            JdkAotCacheTraining.trainingEnvironment(null, java25, false, List.of("/hello")));
        assertEquals(Map.of("JDK_JAVA_OPTIONS", "-Xss1m -XX:AOTCacheOutput=/tmp/app.aot"),
            JdkAotCacheTraining.trainingEnvironment(new String[] {"PATH=/bin", "JDK_JAVA_OPTIONS=-Xss1m"}, java25, false, List.of()));
        assertEquals(Map.of("JDK_JAVA_OPTIONS", "-XX:AOTCacheOutput=/tmp/app.aot -Dmicronaut.application.training.enabled=true "
                + "-Dmicronaut.application.training.warmup.paths[0]=/hello"),
            JdkAotCacheTraining.trainingEnvironment(null, java25, true, List.of("/hello")));
    }

    @Test
    void trainingEnvironmentAddsTheJdk27CreationFlag() {
        var environment = JdkAotCacheTraining.trainingEnvironment(new String[] {"JDK_AOT_VM_OPTIONS=-Xlog:aot"},
            new JdkAotCacheTraining.JavaRuntime(27, true), false, List.of());

        assertEquals("-Xlog:aot -XX:+UnlockDiagnosticVMOptions -XX:+AOTCompatibleOopCompression", environment.get("JDK_AOT_VM_OPTIONS"));
    }

    @Test
    void usesTheFirstExposedTcpPort() {
        assertEquals(Optional.of(8080), JdkAotCacheTraining.firstTcpPort(new ExposedPort[] {ExposedPort.udp(53), ExposedPort.tcp(8080), ExposedPort.tcp(8081)}));
        assertEquals(Optional.empty(), JdkAotCacheTraining.firstTcpPort(null));
        assertEquals(Optional.empty(), JdkAotCacheTraining.firstTcpPort(new ExposedPort[0]));
    }

    @Test
    void fallbackWarmsUpStopsWithSigtermAndCopiesTheCache(@TempDir Path tempDir) throws Exception {
        mockImage(new String[] {"JDK_JAVA_OPTIONS=-Xss1m"}, ExposedPort.tcp(8080));
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(eq("sha256:training"), isNull(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.execInContainer(eq("container"), anyInt(), any(), any(String[].class))).thenAnswer(invocation -> {
            Consumer<String> output = invocation.getArgument(2);
            output.accept("[jdk-aot-cache] GET /hello: 200");
            return 0;
        });
        when(dockerService.awaitExit("container", 180)).thenReturn(143);
        mockCopy("cache");
        Path cacheFile = tempDir.resolve("app.aot");

        training(List.of("/hello")).train("sha256:training", false, cacheFile);

        assertEquals("cache", Files.readString(cacheFile));
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, String>> environment = ArgumentCaptor.forClass(Map.class);
        ArgumentCaptor<String[]> command = ArgumentCaptor.forClass(String[].class);
        InOrder order = inOrder(dockerService);
        order.verify(dockerService).createContainer(eq("sha256:training"), isNull(), eq(false), environment.capture());
        order.verify(dockerService).startContainer("container");
        order.verify(dockerService).execInContainer(eq("container"), eq(360), any(), command.capture());
        order.verify(dockerService).signalContainer("container", "SIGTERM");
        order.verify(dockerService).awaitExit("container", 180);
        order.verify(dockerService).copyFileFromContainer("container", "/tmp/app.aot", cacheFile);
        order.verify(dockerService).removeContainer("container");
        assertEquals(Map.of("JDK_JAVA_OPTIONS", "-Xss1m -XX:AOTCacheOutput=/tmp/app.aot"), environment.getValue());
        var arguments = command.getValue();
        assertEquals(List.of("bash", "-c"), List.of(arguments).subList(0, 2));
        assertEquals(JdkAotCacheTraining.readScript(), arguments[2]);
        assertArrayEquals(new String[] {"training.sh", "warm-up", "8080", "180", "/hello"}, Arrays.copyOfRange(arguments, 3, arguments.length));
        verify(log).info("[jdk-aot-cache] GET /hello: 200");
    }

    @Test
    void switchLetsTheApplicationExitOnItsOwn(@TempDir Path tempDir) throws Exception {
        mockImage(null);
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(eq("sha256:training"), eq("host"), eq(false), anyMap())).thenReturn("container");
        mockCopy("cache");

        new JdkAotCacheTraining(dockerService, log, List.of("/hello"), 180, "host").train("sha256:training", true, tempDir.resolve("app.aot"));

        verify(dockerService).startAndWait("container", "sha256:training", 180);
        verify(dockerService, never()).execInContainer(any(), anyInt(), any(), any(String[].class));
        verify(dockerService, never()).signalContainer(any(), any());
        verify(dockerService).removeContainer("container");
    }

    @Test
    void failsWhenAWarmUpRequestFails(@TempDir Path tempDir) throws Exception {
        mockImage(null, ExposedPort.tcp(8080));
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(any(), any(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.execInContainer(eq("container"), anyInt(), any(), any(String[].class))).thenAnswer(invocation -> {
            Consumer<String> output = invocation.getArgument(2);
            output.accept("[jdk-aot-cache] GET /hello: 200");
            output.accept("[jdk-aot-cache] GET /missing answered 404");
            return 1;
        });

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of("/hello", "/missing")).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertEquals("JDK AOT cache training failed: [jdk-aot-cache] GET /missing answered 404", e.getMessage());
        verify(dockerService).logContainerOutput("container");
        verify(dockerService).removeContainer("container");
        verify(dockerService, never()).signalContainer(any(), any());
    }

    @Test
    void failsClearlyWithoutBash(@TempDir Path tempDir) throws Exception {
        mockImage(null, ExposedPort.tcp(8080));
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(any(), any(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.execInContainer(eq("container"), anyInt(), any(), any(String[].class))).thenReturn(127);

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of()).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertTrue(e.getMessage().contains("the warm-up needs bash in the image"));
    }

    @Test
    void failsWhenTheApplicationDoesNotStopWithSigterm(@TempDir Path tempDir) throws Exception {
        mockImage(null, ExposedPort.tcp(8080));
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(any(), any(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.awaitExit("container", 180)).thenReturn(1);

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of()).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertEquals("JDK AOT cache training failed: the application exited with status 1 after SIGTERM", e.getMessage());
        verify(dockerService).removeContainer("container");
    }

    @Test
    void failsWhenNoCacheWasWritten(@TempDir Path tempDir) throws Exception {
        mockImage(null, ExposedPort.tcp(8080));
        mockJava(JAVA_25_OUTPUT);
        when(dockerService.createContainer(any(), any(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.awaitExit("container", 180)).thenReturn(143);
        doThrow(new IOException("/tmp/app.aot does not exist in container container"))
            .when(dockerService).copyFileFromContainer(eq("container"), eq("/tmp/app.aot"), any());

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of()).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertTrue(e.getMessage().startsWith("JDK AOT cache training failed: the training run did not write the cache"));
        verify(dockerService).logContainerOutput("container");
    }

    @Test
    void failsBeforeTrainingBelowJava25(@TempDir Path tempDir) {
        mockImage(null, ExposedPort.tcp(8080));
        mockJava("openjdk version \"21.0.8\" 2025-07-15");

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of()).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertTrue(e.getMessage().contains("needs Java 25 or later in the image, but the base image runs Java 21"));
        verify(dockerService, never()).createContainer(any(), any(), eq(false), anyMap());
    }

    @Test
    void theFallbackNeedsAnExposedPort(@TempDir Path tempDir) {
        mockImage(null);
        mockJava(JAVA_25_OUTPUT);

        var e = assertThrows(MojoExecutionException.class, () -> training(List.of()).train("sha256:training", false, tempDir.resolve("app.aot")));

        assertTrue(e.getMessage().contains("Expose the port with the Jib container.ports configuration"));
        verify(dockerService, never()).createContainer(any(), any(), eq(false), anyMap());
    }

    @Test
    void theScriptIsShipped() throws IOException {
        String script = JdkAotCacheTraining.readScript();

        assertTrue(script.startsWith("#!/usr/bin/env bash"));
        assertTrue(script.contains("/dev/tcp/127.0.0.1/"));
        assertFalse(script.contains("\r"));
    }

    private JdkAotCacheTraining training(List<String> paths) {
        return new JdkAotCacheTraining(dockerService, log, paths, 180, null);
    }

    private void mockImage(String[] environment, ExposedPort... ports) {
        var config = mock(ContainerConfig.class);
        when(config.getEnv()).thenReturn(environment);
        when(config.getExposedPorts()).thenReturn(ports);
        var image = mock(InspectImageResponse.class);
        when(image.getConfig()).thenReturn(config);
        when(dockerService.inspectImage("sha256:training")).thenReturn(image);
    }

    private void mockJava(String output) {
        try {
            when(dockerService.runAndCaptureOutput(eq("sha256:training"), eq(180), eq(List.of("java",
                "-XX:+UnlockDiagnosticVMOptions", "-XX:+PrintFlagsFinal", "-version"))))
                .thenReturn(new DockerService.ContainerOutput(0, output));
        } catch (IOException e) {
            throw new AssertionError(e);
        }
    }

    private void mockCopy(String content) throws IOException {
        doAnswer(invocation -> {
            Path target = invocation.getArgument(2);
            Files.writeString(target, content);
            return null;
        }).when(dockerService).copyFileFromContainer(eq("container"), eq("/tmp/app.aot"), any());
    }
}
