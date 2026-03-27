package io.micronaut.maven.aot;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

class AbstractMicronautAotCliMojoTest {

    @TempDir
    Path tempDir;

    @Test
    void buildJavaCommandArgumentsPreservesJvmMainAndExtraArgOrder() {
        List<String> arguments = AbstractMicronautAotCliMojo.buildJavaCommandArguments(
            List.of("--enable-preview"),
            "/aot/one.jar:/aot/two.jar",
            "/app/app.jar:/app/lib.jar",
            "example.generated",
            "jit",
            List.of("--verbose", "--config=/tmp/config.properties")
        );

        assertIterableEquals(List.of(
            "--enable-preview",
            "-classpath",
            "/aot/one.jar:/aot/two.jar",
            "io.micronaut.aot.cli.Main",
            "--classpath=/app/app.jar:/app/lib.jar",
            "--package=example.generated",
            "--runtime=jit",
            "--verbose",
            "--config=/tmp/config.properties"
        ), arguments);
    }

    @Test
    void renderJavaArgumentFileEscapesWhitespaceQuotesBackslashesAndHashes() {
        List<String> rendered = AbstractMicronautAotCliMojo.renderJavaArgumentFile(List.of(
            "--enable-preview",
            "C:\\Users\\My User\\workspace\\demo.jar",
            "--classpath=C:\\Users\\My User\\workspace\\demo.jar;C:\\Users\\My User\\workspace\\lib.jar",
            "--message=hello \"quoted\" #value",
            ""
        ));

        assertIterableEquals(List.of(
            "--enable-preview",
            "\"C:\\\\Users\\\\My User\\\\workspace\\\\demo.jar\"",
            "\"--classpath=C:\\\\Users\\\\My User\\\\workspace\\\\demo.jar;C:\\\\Users\\\\My User\\\\workspace\\\\lib.jar\"",
            "\"--message=hello \\\"quoted\\\" #value\"",
            "\"\""
        ), rendered);
    }

    @Test
    void writeJavaArgumentFileWritesOneEscapedArgumentPerLine() throws Exception {
        File argumentFile = AbstractMicronautAotCliMojo.writeJavaArgumentFile(
            tempDir.toFile(),
            List.of("--enable-preview", "--package=hello world")
        );

        assertEquals(String.join(System.lineSeparator(), List.of(
            "--enable-preview",
            "\"--package=hello world\""
        )) + System.lineSeparator(), Files.readString(argumentFile.toPath()));
    }
}
