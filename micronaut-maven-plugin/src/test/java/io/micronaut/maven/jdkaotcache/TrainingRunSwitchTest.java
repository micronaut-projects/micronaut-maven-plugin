package io.micronaut.maven.jdkaotcache;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static io.micronaut.maven.jdkaotcache.MicronautJars.APPLICATION_CONFIGURATION;
import static io.micronaut.maven.jdkaotcache.MicronautJars.WARMUP_CONFIGURATION;
import static io.micronaut.maven.jdkaotcache.MicronautJars.artifact;
import static io.micronaut.maven.jdkaotcache.MicronautJars.jar;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

class TrainingRunSwitchTest {

    @Test
    void detectsTheSwitchInMicronautContext(@TempDir Path tempDir) throws IOException {
        var context = artifact("micronaut-context", jar(tempDir, "context.jar",
            Map.of(APPLICATION_CONFIGURATION, "micronaut.application.name\u0001micronaut.application.training.enabled")));

        assertTrue(TrainingRunSwitch.isAvailable(List.of(context), false));
        assertFalse(TrainingRunSwitch.isAvailable(List.of(context), true));
    }

    @Test
    void theWarmUpNeedsTheSwitchInMicronautHttpServer(@TempDir Path tempDir) throws IOException {
        var context = artifact("micronaut-context", jar(tempDir, "context.jar",
            Map.of(APPLICATION_CONFIGURATION, "micronaut.application.training.enabled")));
        var httpServer = artifact("micronaut-http-server", jar(tempDir, "http-server.jar",
            Map.of(WARMUP_CONFIGURATION, "micronaut.application.training.warmup")));

        assertTrue(TrainingRunSwitch.isAvailable(List.of(context, httpServer), true));
    }

    @Test
    void olderMicronautVersionsHaveNoSwitch(@TempDir Path tempDir) throws IOException {
        var context = artifact("micronaut-context", jar(tempDir, "context.jar",
            Map.of(APPLICATION_CONFIGURATION, "micronaut.application.name")));
        var httpServer = artifact("micronaut-http-server", jar(tempDir, "http-server.jar",
            Map.of("io/micronaut/http/server/HttpServerConfiguration.class", "micronaut.server")));

        assertFalse(TrainingRunSwitch.isAvailable(List.of(context, httpServer), false));
        assertFalse(TrainingRunSwitch.isAvailable(List.of(httpServer), false));
        assertFalse(TrainingRunSwitch.isAvailable(List.of(), false));
    }

    @Test
    void ignoresOtherGroupsAndMissingFiles(@TempDir Path tempDir) throws IOException {
        var jar = jar(tempDir, "context.jar", Map.of(APPLICATION_CONFIGURATION, "micronaut.application.training.enabled"));
        var otherGroup = artifact("micronaut-context", jar);
        when(otherGroup.getGroupId()).thenReturn("com.example");
        var missingFile = artifact("micronaut-context", tempDir.resolve("missing.jar"));

        assertFalse(TrainingRunSwitch.isAvailable(List.of(otherGroup), false));
        assertFalse(TrainingRunSwitch.isAvailable(List.of(missingFile), false));
    }

    @Test
    void detectsTheTrainingModeInMicronautContext(@TempDir Path tempDir) throws IOException {
        assertTrue(TrainingRunSwitch.hasLoadMode(MicronautJars.withLoadMode(tempDir)));
        assertTrue(TrainingRunSwitch.hasLoadMode(List.of(MicronautJars.contextWithLoadMode(tempDir))));
    }

    @Test
    void versionsWithoutTheModePropertyHaveNoTrainingMode(@TempDir Path tempDir) throws IOException {
        var modeWithoutTheSwitch = artifact("micronaut-context", jar(tempDir, "context.jar",
            Map.of(APPLICATION_CONFIGURATION, "micronaut.application.training.mode")));
        var modeInAnotherClass = artifact("micronaut-context", jar(tempDir, "other.jar", Map.of(
            APPLICATION_CONFIGURATION, "micronaut.application.training.enabled",
            "io/micronaut/runtime/Micronaut.class", "micronaut.application.training.mode")));
        var withoutTheClass = artifact("micronaut-context", jar(tempDir, "empty.jar",
            Map.of("io/micronaut/runtime/Micronaut.class", "micronaut.application.training.enabled\u0001micronaut.application.training.mode")));
        var otherGroup = MicronautJars.contextWithLoadMode(tempDir);
        when(otherGroup.getGroupId()).thenReturn("com.example");

        assertFalse(TrainingRunSwitch.hasLoadMode(MicronautJars.withSwitch(tempDir)));
        assertFalse(TrainingRunSwitch.hasLoadMode(MicronautJars.withoutSwitch(tempDir)));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of(modeWithoutTheSwitch)));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of(modeInAnotherClass)));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of(withoutTheClass)));
        assertFalse(TrainingRunSwitch.isAvailable(List.of(withoutTheClass), false));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of(otherGroup)));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of(artifact("micronaut-context", tempDir.resolve("missing.jar")))));
        assertFalse(TrainingRunSwitch.hasLoadMode(List.of()));
    }

    @Test
    void theStandInsMatchTheSwitchDetection(@TempDir Path tempDir) throws IOException {
        assertTrue(TrainingRunSwitch.isAvailable(MicronautJars.withLoadMode(tempDir), true));
        assertTrue(TrainingRunSwitch.isAvailable(MicronautJars.withSwitch(tempDir), true));
        assertFalse(TrainingRunSwitch.isAvailable(MicronautJars.withoutSwitch(tempDir), false));
    }

    @Test
    void passesTheModeAndTheWarmUpPathsAsIndexedPropertiesSoThatCommasAreKept() {
        assertEquals(List.of(
            "-Dmicronaut.application.training.enabled=true",
            "-Dmicronaut.application.training.mode=start",
            "-Dmicronaut.application.training.warmup.paths[0]=/hello",
            "-Dmicronaut.application.training.warmup.paths[1]=/books?ids=1,2"
        ), TrainingRunSwitch.systemProperties(TrainingMode.START, List.of("/hello", "/books?ids=1,2")));
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true", "-Dmicronaut.application.training.mode=start"),
            TrainingRunSwitch.systemProperties(TrainingMode.START, List.of()));
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true", "-Dmicronaut.application.training.mode=load"),
            TrainingRunSwitch.systemProperties(TrainingMode.LOAD, List.of()));
    }
}
