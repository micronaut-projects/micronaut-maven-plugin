package io.micronaut.maven.aot;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class AbstractAotAnalysisMojoTest {

    @Test
    void onSuccessDoesNotDeleteFilesOutsideOutputDirectoryWhenResourceFilterContainsTraversal(@TempDir Path tempDir) throws Exception {
        Path outputDirectory = tempDir.resolve("target/classes");
        Path generatedDirectory = tempDir.resolve("aot/jit/generated");
        Path outsideFile = tempDir.resolve("outside.txt");

        TestAotAnalysisMojo mojo = newMojo(tempDir, outputDirectory);
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

        TestAotAnalysisMojo mojo = newMojo(tempDir, outputDirectory);
        Files.createDirectories(generatedDirectory.resolve("classes"));
        Files.createDirectories(generatedDirectory.resolve("logs"));
        Files.createDirectories(outputDirectory);
        Files.writeString(generatedDirectory.resolve("logs/resource-filter.txt"), "\n   \n.\n");
        Files.writeString(insideFile, "inside");

        mojo.onSuccess(tempDir.resolve("aot/jit").toFile());

        assertTrue(Files.exists(insideFile));
    }

    @Test
    void getExtraArgsUsesDefaultConfigFileWhenNoExplicitConfigIsProvided(@TempDir Path tempDir) throws Exception {
        Path targetDirectory = tempDir.resolve("target");
        Path outputDirectory = targetDirectory.resolve("classes");
        Files.createDirectories(targetDirectory);
        Files.createDirectories(targetDirectory.resolve("aot/jit"));
        Files.writeString(targetDirectory.resolve("aot.properties"), "foo=bar\n");

        TestAotAnalysisMojo mojo = newMojo(tempDir, outputDirectory);
        setField(AbstractAotAnalysisMojo.class, mojo, "configFile", null);

        List<String> extraArgs = mojo.getExtraArgs();
        File effectiveConfig = new File(extraArgs.get(3));
        Properties props = new Properties();
        try (var in = Files.newInputStream(effectiveConfig.toPath())) {
            props.load(in);
        }

        assertEquals("--output", extraArgs.get(0));
        assertEquals("--config", extraArgs.get(2));
        assertEquals("bar", props.getProperty("foo"));
        assertTrue(props.containsKey("known.missing.types.list"));
    }

    private static TestAotAnalysisMojo newMojo(Path projectDirectory, Path outputDirectory) throws Exception {
        MavenProject project = new MavenProject();
        Build build = new Build();
        build.setDirectory(projectDirectory.resolve("target").toString());
        project.setBuild(build);
        project.setPackaging("jar");

        TestAotAnalysisMojo mojo = new TestAotAnalysisMojo(
            mock(CompilerService.class),
            mock(ExecutorService.class),
            project,
            mock(DependencyResolutionService.class),
            mock(MavenSession.class),
            mock(ToolchainManager.class)
        );
        mojo.outputDirectory = outputDirectory.toFile();
        mojo.runtime = "jit";
        Files.createDirectories(outputDirectory);
        setField(AbstractAotAnalysisMojo.class, mojo, "baseDirectory", projectDirectory.resolve("target").toFile());
        setField(AbstractAotAnalysisMojo.class, mojo, "configFile", projectDirectory.resolve("target/aot.properties").toFile());
        return mojo;
    }

    private static void setField(Class<?> owner, Object target, String name, Object value) throws Exception {
        Field field = owner.getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static final class TestAotAnalysisMojo extends AbstractAotAnalysisMojo {

        private TestAotAnalysisMojo(CompilerService compilerService,
                                    ExecutorService executorService,
                                    MavenProject mavenProject,
                                    DependencyResolutionService dependencyResolutionService,
                                    MavenSession mavenSession,
                                    ToolchainManager toolchainManager) {
            super(compilerService, executorService, mavenProject, dependencyResolutionService, mavenSession, toolchainManager);
        }
    }
}
