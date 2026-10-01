package io.micronaut.maven.jdkaotcache;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs the {@code train} command of the training script, which the generated Dockerfile runs, with a stand-in for
 * {@code java} that records the options it is started with and exits, as an application does when the Micronaut
 * training-run switch ends its run.
 */
@EnabledOnOs({OS.LINUX, OS.MAC})
class TrainingScriptTest {

    private static final String JAVA = """
        #!/usr/bin/env bash
        directory=$(dirname "$0")
        for argument in "$@"; do
            if [ "$argument" = "-version" ]; then
                printf '%s\\n' "$@" > "$directory/version-arguments"
                echo 'openjdk version "25.0.4" 2026-07-21 LTS' >&2
                exit 0
            fi
        done
        printf '%s' "$JDK_JAVA_OPTIONS" > "$directory/options"
        printf '%s\\n' "$@" > "$directory/arguments"
        cache_option='-XX:AOTCacheOutput=([^ ]+)'
        if [ ! -f "$directory/no-cache" ] && [[ $JDK_JAVA_OPTIONS =~ $cache_option ]]; then
            printf 'cache' > "${BASH_REMATCH[1]}"
        fi
        exit "$(cat "$directory/status")"
        """;

    @TempDir
    Path tempDir;

    private Path script;
    private Path java;
    private Path cache;

    @BeforeEach
    void writeTheScriptAndTheJavaStandIn() throws IOException {
        script = Files.writeString(tempDir.resolve("training.sh"), JdkAotCacheTraining.readScript());
        java = Files.writeString(tempDir.resolve("java"), JAVA);
        assertTrue(java.toFile().setExecutable(true));
        Files.writeString(tempDir.resolve("status"), "0");
        cache = tempDir.resolve("app.aot");
    }

    @Test
    void loadRunSelectsTheLoadModeAndWaitsForTheApplicationToExit() throws Exception {
        Result result = train("load");

        assertEquals(0, result.status(), result.output());
        assertEquals("-XX:AOTCacheOutput=" + cache + " -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=load", Files.readString(tempDir.resolve("options")));
        assertEquals(List.of("-cp", "@/home/app/classpath", "example.Application"), Files.readAllLines(tempDir.resolve("arguments")));
        assertEquals("cache", Files.readString(cache));
        assertTrue(result.output().contains("[jdk-aot-cache] Wrote the JDK AOT cache " + cache), result.output());
        assertFalse(result.output().contains("Waiting up to"), result.output());
        assertFalse(result.output().contains("SIGTERM"), result.output());
    }

    @Test
    void loadRunKeepsTheOptionsOfTheImage() throws Exception {
        Result result = train(List.of("JDK_JAVA_OPTIONS=-Xss1m"), "load");

        assertEquals(0, result.status(), result.output());
        assertEquals("-Xss1m -XX:AOTCacheOutput=" + cache + " -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=load", Files.readString(tempDir.resolve("options")));
    }

    @Test
    void switchRunSelectsTheStartModeAndPassesThePaths() throws Exception {
        Result result = train("switch", "/hello", "/books?ids=1,2");

        assertEquals(0, result.status(), result.output());
        assertEquals("-XX:AOTCacheOutput=" + cache + " -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=start -Dmicronaut.application.training.warmup.paths[0]=/hello "
            + "-Dmicronaut.application.training.warmup.paths[1]=/books?ids=1,2", Files.readString(tempDir.resolve("options")));
    }

    @Test
    void noJvmOfTheTrainingWritesPerformanceData() throws Exception {
        // The layer of the RUN instruction would keep a /tmp/hsperfdata_<user>/<pid> file
        Result result = train("switch");

        assertEquals(0, result.status(), result.output());
        assertEquals(List.of("-XX:-UsePerfData", "-XX:+UnlockDiagnosticVMOptions", "-XX:+PrintFlagsFinal", "-version"),
            Files.readAllLines(tempDir.resolve("version-arguments")));
        assertEquals("-XX:AOTCacheOutput=" + cache + " -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=start", Files.readString(tempDir.resolve("options")));
        assertTrue(result.output().contains("[jdk-aot-cache] Training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=" + cache
            + " -XX:-UsePerfData "), result.output());
    }

    @Test
    void loadRunThatFailsFailsTheTraining() throws Exception {
        Files.writeString(tempDir.resolve("status"), "1");

        Result result = train("load");

        assertNotEquals(0, result.status(), result.output());
        assertTrue(result.output().contains("[jdk-aot-cache] The training run exited with status 1"), result.output());
    }

    @Test
    void loadRunThatWritesNoCacheFailsTheTraining() throws Exception {
        Files.writeString(tempDir.resolve("no-cache"), "");

        Result result = train("load");

        assertNotEquals(0, result.status(), result.output());
        assertTrue(result.output().contains("[jdk-aot-cache] The training run did not write the JDK AOT cache " + cache), result.output());
    }

    @Test
    void loadRunRejectsPathsBeforeItRunsTheApplication() throws Exception {
        Result result = train("load", "/hello");

        assertNotEquals(0, result.status(), result.output());
        assertTrue(result.output().contains("[jdk-aot-cache] A load training run does not start the application, so it "
            + "cannot send requests to /hello"), result.output());
        assertFalse(Files.exists(tempDir.resolve("options")));
    }

    @Test
    void anUnknownRunIsRejectedBeforeItRunsTheApplication() throws Exception {
        Result result = train("laod");

        assertNotEquals(0, result.status(), result.output());
        assertTrue(result.output().contains("[jdk-aot-cache] Unknown training run 'laod': it must be load, switch or sigterm"),
            result.output());
        assertFalse(Files.exists(tempDir.resolve("options")));
    }

    private Result train(String run, String... paths) throws IOException, InterruptedException {
        return train(List.of(), run, paths);
    }

    private Result train(List<String> environment, String run, String... paths) throws IOException, InterruptedException {
        var command = new ArrayList<>(List.of("bash", script.toString(), "train", cache.toString(), "8080", "30", run));
        command.addAll(List.of(paths));
        command.addAll(List.of("--", java.toString(), "-cp", "@/home/app/classpath", "example.Application"));
        var builder = new ProcessBuilder(command).redirectErrorStream(true);
        builder.environment().remove("JDK_JAVA_OPTIONS");
        builder.environment().remove("JDK_AOT_VM_OPTIONS");
        for (String variable : environment) {
            String[] nameAndValue = variable.split("=", 2);
            builder.environment().put(nameAndValue[0], nameAndValue[1]);
        }
        Process process = builder.start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assertTrue(process.waitFor(60, TimeUnit.SECONDS), output);
        return new Result(process.exitValue(), output);
    }

    private record Result(int status, String output) {
    }
}
