package io.micronaut.maven.jdkaotcache;

import org.apache.maven.artifact.Artifact;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class TrainingRunSwitchTest {

    private static final String APPLICATION_CONFIGURATION = "io/micronaut/runtime/ApplicationConfiguration.class";
    private static final String WARMUP_CONFIGURATION = "io/micronaut/http/server/TrainingWarmupConfiguration.class";

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
    void passesTheWarmUpPathsAsIndexedPropertiesSoThatCommasAreKept() {
        assertEquals(List.of(
            "-Dmicronaut.application.training.enabled=true",
            "-Dmicronaut.application.training.warmup.paths[0]=/hello",
            "-Dmicronaut.application.training.warmup.paths[1]=/books?ids=1,2"
        ), TrainingRunSwitch.systemProperties(List.of("/hello", "/books?ids=1,2")));
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true"), TrainingRunSwitch.systemProperties(List.of()));
    }

    private static Artifact artifact(String artifactId, Path file) {
        var artifact = mock(Artifact.class);
        when(artifact.getGroupId()).thenReturn("io.micronaut");
        when(artifact.getArtifactId()).thenReturn(artifactId);
        when(artifact.getFile()).thenReturn(file.toFile());
        return artifact;
    }

    private static Path jar(Path directory, String name, Map<String, String> entries) throws IOException {
        Path jar = directory.resolve(name);
        try (var out = new JarOutputStream(Files.newOutputStream(jar))) {
            for (var entry : entries.entrySet()) {
                out.putNextEntry(new JarEntry(entry.getKey()));
                out.write(("Êþº¾" + entry.getValue()).getBytes(StandardCharsets.ISO_8859_1));
                out.closeEntry();
            }
        }
        return jar;
    }
}
