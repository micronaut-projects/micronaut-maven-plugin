package io.micronaut.maven.classpath;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.jar.JarFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ClassPathTransformLauncherTest {

    @Test
    void rewritesAThirdPartyJarAndKeepsItsName(@TempDir Path tempDir) throws Exception {
        Path jar = Path.of(StringUtils.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        Path output = tempDir.resolve("output");
        Files.createDirectories(output);
        Path request = tempDir.resolve("request.txt");
        Files.write(request, List.of(
            "output\t" + output,
            "desugar\ttrue",
            "strip\ttrue",
            "entry\t" + jar,
            "stripped\t" + jar
        ), StandardCharsets.UTF_8);

        ClassPathTransformLauncher.main(new String[] {request.toString()});

        List<String> mapping = Files.readAllLines(output.resolve(ClassPathTransformService.CLASS_PATH_FILE));
        assertEquals(1, mapping.size());
        String[] entry = mapping.get(0).split("\t");
        assertEquals(jar.toAbsolutePath().toString(), entry[0]);
        Path copy = Path.of(entry[1]);
        assertNotEquals(jar.toAbsolutePath(), copy);
        assertEquals(jar.getFileName(), copy.getFileName());
        try (var copyJar = new JarFile(copy.toFile())) {
            assertTrue(copyJar.stream().anyMatch(e -> e.getName().contains("$$Lambda$R")));
        }
        assertTrue(Files.readString(output.resolve(ClassPathTransformService.REPORT_FILE)).contains("Desugared"));
    }

    @Test
    void anOptionsRecordWithoutAStepIsDisabled() {
        assertTrue(new ClassPathTransformOptions(true, false, "1").enabled());
        assertTrue(new ClassPathTransformOptions(false, true, "1").enabled());
        assertEquals(false, new ClassPathTransformOptions(false, false, "1").enabled());
    }
}
