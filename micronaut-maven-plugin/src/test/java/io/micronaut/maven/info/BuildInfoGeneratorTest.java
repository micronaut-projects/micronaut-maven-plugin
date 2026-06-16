package io.micronaut.maven.info;

import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BuildInfoGeneratorTest {

    @Test
    void generatesConservativeBuildProperties() {
        MavenProject project = project();
        project.getProperties().setProperty("maven.compiler.source", "17");
        project.getProperties().setProperty("maven.compiler.target", "17");
        project.getProperties().setProperty("micronaut.version", "5.0.2");

        Map<String, String> properties = BuildInfoGenerator.generate(
            project,
            Instant.parse("2026-01-02T03:04:05Z"),
            Map.of("build.number", "42")
        );

        assertEquals("io.micronaut.test", properties.get("build.group"));
        assertEquals("demo", properties.get("build.artifact"));
        assertEquals("Demo", properties.get("build.name"));
        assertEquals("1.0.0", properties.get("build.version"));
        assertEquals("2026-01-02T03:04:05Z", properties.get("build.time"));
        assertEquals("17", properties.get("build.java.source"));
        assertEquals("17", properties.get("build.java.target"));
        assertEquals("5.0.2", properties.get("build.micronaut.version"));
        assertEquals("42", properties.get("build.number"));
    }

    @Test
    void ignoresAdditionalBuildPropertiesWithBlankKeys() {
        Map<String, String> properties = BuildInfoGenerator.generate(
            project(),
            Instant.parse("2026-01-02T03:04:05Z"),
            Map.of(" ", "ignored", "build.valid", "included")
        );

        assertFalse(properties.containsKey(" "));
        assertEquals("included", properties.get("build.valid"));
    }

    @Test
    void outputTimestampTakesPrecedenceForReproducibleBuilds() {
        MavenProject project = project();
        project.getProperties().setProperty("project.build.outputTimestamp", "2026-02-03T04:05:06Z");

        assertEquals(Instant.parse("2026-02-03T04:05:06Z"), BuildInfoGenerator.resolveBuildTime(null, project, null));
    }

    @Test
    void writesSortedPropertiesWithoutTimestampComment(@TempDir Path tempDir) throws IOException {
        Path output = tempDir.resolve("build-info.properties");

        PropertiesFileWriter.write(output, Map.of(
            "z", "last",
            "a key", "value:with=special#chars"
        ));

        String content = Files.readString(output);
        assertTrue(content.startsWith("a\\ key=value\\:with\\=special\\#chars"));
        assertTrue(content.contains("\nz=last"));
        assertFalse(content.startsWith("#"));
    }

    private MavenProject project() {
        var project = new MavenProject();
        project.setGroupId("io.micronaut.test");
        project.setArtifactId("demo");
        project.setName("Demo");
        project.setVersion("1.0.0");
        var build = new Build();
        build.setOutputDirectory("target/classes");
        project.setBuild(build);
        return project;
    }
}
