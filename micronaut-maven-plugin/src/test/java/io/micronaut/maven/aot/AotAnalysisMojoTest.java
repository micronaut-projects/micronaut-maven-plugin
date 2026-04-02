package io.micronaut.maven.aot;

import io.micronaut.maven.aot.internal.AotCompilerService;
import io.micronaut.maven.aot.internal.AotDependencyResolutionService;
import io.micronaut.maven.aot.internal.AotExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class AotAnalysisMojoTest {

    @Test
    void onSuccessDoesNotDeleteFilesOutsideOutputDirectoryWhenResourceFilterContainsTraversal(@TempDir Path tempDir) throws Exception {
        Path outputDirectory = tempDir.resolve("target/classes");
        Path generatedDirectory = tempDir.resolve("aot/jit/generated");
        Path outsideFile = tempDir.resolve("outside.txt");

        AotAnalysisMojo mojo = newMojo(outputDirectory);
        Files.createDirectories(generatedDirectory.resolve("classes"));
        Files.createDirectories(generatedDirectory.resolve("logs"));
        Files.writeString(generatedDirectory.resolve("logs/resource-filter.txt"), "../../outside.txt");
        Files.writeString(outsideFile, "outside");

        mojo.onSuccess(tempDir.resolve("aot/jit").toFile());

        assertTrue(Files.exists(outsideFile));
    }

    @Test
    void onSuccessIgnoresBlankAndSelfDeletionEntries(@TempDir Path tempDir) throws Exception {
        Path outputDirectory = tempDir.resolve("target/classes");
        Path generatedDirectory = tempDir.resolve("aot/jit/generated");
        Path insideFile = outputDirectory.resolve("keep.txt");

        AotAnalysisMojo mojo = newMojo(outputDirectory);
        Files.createDirectories(generatedDirectory.resolve("classes"));
        Files.createDirectories(generatedDirectory.resolve("logs"));
        Files.createDirectories(outputDirectory);
        Files.writeString(generatedDirectory.resolve("logs/resource-filter.txt"), "\n   \n.\n");
        Files.writeString(insideFile, "inside");

        mojo.onSuccess(tempDir.resolve("aot/jit").toFile());

        assertTrue(Files.exists(insideFile));
    }

    private static AotAnalysisMojo newMojo(Path outputDirectory) throws Exception {
        var mojo = new AotAnalysisMojo(
            mock(AotCompilerService.class),
            mock(AotExecutorService.class),
            mock(MavenProject.class),
            mock(AotDependencyResolutionService.class),
            mock(MavenSession.class),
            mock(ToolchainManager.class)
        );

        mojo.outputDirectory = outputDirectory.toFile();
        Files.createDirectories(outputDirectory);
        return mojo;
    }
}
