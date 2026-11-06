package io.micronaut.maven.logback;

import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.zip.CRC32;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PrecompileLogbackMojoTest {

    @TempDir
    Path target;

    @Test
    void removesOnlyUnchangedEarlierOutputWhenDisabled() throws Exception {
        Path classes = Files.createDirectories(target.resolve("classes"));
        Path configurator = classes.resolve("io/micronaut/aot/logback/generated/LogbackConfigurator.class");
        Path service = classes.resolve("META-INF/services/ch.qos.logback.classic.spi.Configurator");
        Files.createDirectories(configurator.getParent());
        Files.createDirectories(service.getParent());
        byte[] generated = {1, 2, 3};
        Files.write(configurator, generated);
        Files.writeString(service, "io.micronaut.aot.logback.generated.LogbackConfigurator\n");
        Path record = target.resolve(PrecompileLogbackMojo.RECORD_FILE);
        Files.createDirectories(record.getParent());
        Files.write(record, List.of(
            "io/micronaut/aot/logback/generated/LogbackConfigurator.class\t3\t" + crc(generated),
            "META-INF/services/ch.qos.logback.classic.spi.Configurator\t4\t0"
        ), StandardCharsets.UTF_8);

        MavenProject project = mock(MavenProject.class);
        Build build = new Build();
        build.setDirectory(target.toString());
        build.setOutputDirectory(classes.toString());
        when(project.getBuild()).thenReturn(build);

        new PrecompileLogbackMojo(project).execute();

        assertFalse(Files.exists(configurator));
        // Rewritten since it was recorded, as aot-analysis does: it stays
        assertTrue(Files.exists(service));
        assertFalse(Files.exists(record));
    }

    private static long crc(byte[] content) {
        var crc = new CRC32();
        crc.update(content);
        return crc.getValue();
    }
}
