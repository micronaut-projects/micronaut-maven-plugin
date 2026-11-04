package io.micronaut.maven.jdkaotcache;

import org.apache.maven.artifact.Artifact;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Stand-ins for the Micronaut JARs of an application, with the property names that the plugin looks for in them.
 */
public final class MicronautJars {

    static final String APPLICATION_CONFIGURATION = "io/micronaut/runtime/ApplicationConfiguration.class";
    static final String WARMUP_CONFIGURATION = "io/micronaut/http/server/TrainingWarmupConfiguration.class";

    private MicronautJars() {
    }

    /**
     * @return the dependencies of an application whose Micronaut version has neither the training-run switch nor the
     * training mode, as every released version
     */
    public static List<Artifact> withoutSwitch(Path directory) throws IOException {
        return List.of(
            artifact("micronaut-context", jar(directory, "context-released.jar", Map.of(APPLICATION_CONFIGURATION, "micronaut.application.name"))),
            artifact("micronaut-http-server", jar(directory, "http-server-released.jar", Map.of("io/micronaut/http/server/HttpServerConfiguration.class", "micronaut.server"))));
    }

    /**
     * @return the dependencies of an application whose Micronaut version has the training-run switch and the warm-up,
     * but no training mode
     */
    public static List<Artifact> withSwitch(Path directory) throws IOException {
        return List.of(
            artifact("micronaut-context", jar(directory, "context-switch.jar",
                Map.of(APPLICATION_CONFIGURATION, "micronaut.application.name\u0001micronaut.application.training.enabled"))),
            httpServerWithWarmUp(directory));
    }

    /**
     * @return the dependencies of an application whose Micronaut version has the training-run switch, the warm-up and
     * the training mode
     */
    public static List<Artifact> withLoadMode(Path directory) throws IOException {
        return List.of(contextWithLoadMode(directory), httpServerWithWarmUp(directory));
    }

    static Artifact contextWithLoadMode(Path directory) throws IOException {
        return artifact("micronaut-context", jar(directory, "context-mode.jar", Map.of(APPLICATION_CONFIGURATION,
            "micronaut.application.name\u0001&micronaut.application.training.enabled\u0001#micronaut.application.training.mode")));
    }

    static Artifact httpServerWithWarmUp(Path directory) throws IOException {
        return artifact("micronaut-http-server", jar(directory, "http-server-warm-up.jar",
            Map.of(WARMUP_CONFIGURATION, "micronaut.application.training.warmup")));
    }

    static Artifact artifact(String artifactId, Path file) {
        var artifact = mock(Artifact.class);
        when(artifact.getGroupId()).thenReturn("io.micronaut");
        when(artifact.getArtifactId()).thenReturn(artifactId);
        when(artifact.getFile()).thenReturn(file.toFile());
        return artifact;
    }

    static Path jar(Path directory, String name, Map<String, String> entries) throws IOException {
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
