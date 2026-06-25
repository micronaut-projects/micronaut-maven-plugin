package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class BuildpackMojoTest {

    @Test
    void buildsPackInvocationFromBuildpackConfiguration(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.runImage = "paketobuildpacks/run-jammy-base:latest";
        mojo.imageName = "registry.example.com/team/app:1.0";
        mojo.environment = java.util.Map.of("BP_JVM_VERSION", "25.*");
        mojo.trustBuilder = true;

        List<String> command = mojo.buildInvocation().command();

        assertEquals(List.of(
            "pack",
            "build",
            "registry.example.com/team/app:1.0",
            "--builder",
            "paketobuildpacks/builder-jammy-base:latest",
            "--path",
            applicationJar.toAbsolutePath().toString(),
            "--run-image",
            "paketobuildpacks/run-jammy-base:latest",
            "--trust-builder",
            "--env",
            "BP_JVM_VERSION=25.*"
        ), command);
    }

    @Test
    void readsBuildpackEnvironmentFromUserProperties(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var userProperties = new Properties();
        userProperties.setProperty("micronaut.buildpack.environment.BP_JVM_VERSION", "25.*");
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), userProperties, new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        List<String> command = mojo.buildInvocation().command();

        assertTrue(command.contains("BP_JVM_VERSION=25.*"));
    }

    @Test
    void fallsBackToJibToImageWhenImageNameIsOmitted(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.of("registry.example.com/team/app:2.0"), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";

        List<String> command = mojo.buildInvocation().command();

        assertEquals("registry.example.com/team/app:2.0", command.get(2));
    }

    @Test
    void requiresBuilderImage(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("micronaut.buildpack.builder-image"));
    }

    @Test
    void requiresImageNameOrJibFallback(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("micronaut.buildpack.image-name"));
        assertTrue(exception.getMessage().contains("jib.to.image"));
    }

    @Test
    void rejectsInvalidImageReference(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "not a valid image";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("micronaut.buildpack.builder-image is not a valid Docker image reference"));
    }

    @Test
    void requiresExistingApplicationArtifact(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir, tempDir.resolve("missing.jar"), Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("Buildpack application artifact does not exist"));
    }

    @Test
    void executesPackAfterVersionCheck(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var runner = new RecordingRunner(0);
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), runner);
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        mojo.execute();

        assertEquals("pack", runner.verifiedExecutable);
        assertEquals(tempDir.toFile(), runner.verifiedWorkingDirectory);
        assertFalse(runner.commands.isEmpty());
        assertEquals("pack", runner.commands.get(0).get(0));
        assertEquals("build", runner.commands.get(0).get(1));
    }

    @Test
    void failsWhenPackExecutableCannotBeChecked(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new FailingVerifyRunner());
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Unable to execute the pack CLI"));
    }

    @Test
    void failsWhenPackBuildExitsWithNonZeroStatus(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(3));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("pack build failed with exit code 3"));
    }

    @Test
    void failsWhenPackBuildCannotStart(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new FailingRunRunner(new IOException("missing")));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Unable to run pack build"));
    }

    @Test
    void preservesInterruptedStatusWhenPackVersionCheckIsInterrupted(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new InterruptedVerifyRunner());
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Interrupted while checking the pack CLI"));
        assertTrue(Thread.interrupted());
    }

    @Test
    void preservesInterruptedStatusWhenPackBuildIsInterrupted(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new FailingRunRunner(new InterruptedException("stop")));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Interrupted while running pack build"));
        assertTrue(Thread.interrupted());
    }

    @Test
    void resolvesEnvironmentFromProjectSystemUserAndPluginConfiguration(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var projectProperties = new Properties();
        projectProperties.setProperty("micronaut.buildpack.environment.BP_JVM_VERSION", "17.*");
        var systemProperties = new Properties();
        systemProperties.setProperty("micronaut.buildpack.environment.BP_JVM_VERSION", "21.*");
        systemProperties.setProperty("micronaut.buildpack.environment.BP_LOG_LEVEL", "debug");
        var userProperties = new Properties();
        userProperties.setProperty("micronaut.buildpack.environment.BP_JVM_VERSION", "25.*");
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), projectProperties, systemProperties, userProperties, new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";
        mojo.environment = Map.of("BP_NATIVE_IMAGE", "false");

        List<String> command = mojo.buildInvocation().command();

        assertTrue(command.contains("BP_JVM_VERSION=25.*"));
        assertTrue(command.contains("BP_LOG_LEVEL=debug"));
        assertTrue(command.contains("BP_NATIVE_IMAGE=false"));
    }

    @Test
    void rejectsInvalidEnvironmentVariableName(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("app.jar"));
        var mojo = newMojo(tempDir, applicationJar, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";
        mojo.environment = Map.of("BP-JVM-VERSION", "25.*");

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("Buildpack environment variable names must match"));
    }

    @Test
    void usesConfiguredRelativeArtifact(@TempDir Path tempDir) throws Exception {
        Path applicationJar = Files.createFile(tempDir.resolve("custom.jar"));
        var mojo = newMojo(tempDir, tempDir.resolve("ignored.jar"), Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";
        mojo.artifact = new File("custom.jar");

        List<String> command = mojo.buildInvocation().command();

        assertTrue(command.contains(applicationJar.toAbsolutePath().toString()));
    }

    @Test
    void fallsBackToBuildFinalNameWhenProjectArtifactIsUnavailable(@TempDir Path tempDir) throws Exception {
        Path target = Files.createDirectory(tempDir.resolve("target"));
        Path applicationJar = Files.createFile(target.resolve("app.jar"));
        var mojo = newMojo(tempDir, null, Optional.empty(), new Properties(), new RecordingRunner(0));
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";
        mojo.imageName = "example.com/app:latest";

        List<String> command = mojo.buildInvocation().command();

        assertTrue(command.contains(applicationJar.toAbsolutePath().toString()));
    }

    @Test
    void requiresArtifactWhenProjectBuildIsUnavailable(@TempDir Path tempDir) {
        var project = mock(MavenProject.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        var session = mock(MavenSession.class);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(new Properties());
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getToImage()).thenReturn(Optional.of("example.com/app:latest"));
        var mojo = new BuildpackMojo(project, session, jibConfigurationService, new RecordingRunner(0));
        mojo.packExecutable = BuildpackMojo.DEFAULT_PACK_EXECUTABLE;
        mojo.builderImage = "paketobuildpacks/builder-jammy-base:latest";

        var exception = assertThrows(MojoExecutionException.class, mojo::buildInvocation);

        assertTrue(exception.getMessage().contains("Unable to determine the buildpack application artifact"));
    }

    private static BuildpackMojo newMojo(Path tempDir, Path applicationJar, Optional<String> jibToImage,
                                         Properties userProperties, BuildpackMojo.BuildpackCommandRunner runner) {
        return newMojo(tempDir, applicationJar, jibToImage, new Properties(), new Properties(), userProperties, runner);
    }

    private static BuildpackMojo newMojo(Path tempDir, Path applicationJar, Optional<String> jibToImage,
                                         Properties projectProperties, Properties systemProperties,
                                         Properties userProperties, BuildpackMojo.BuildpackCommandRunner runner) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        var artifact = mock(Artifact.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        when(project.getProperties()).thenReturn(projectProperties);
        when(project.getBuild()).thenReturn(build);
        when(project.getArtifact()).thenReturn(artifact);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        when(build.getFinalName()).thenReturn("app");
        when(artifact.getFile()).thenReturn(applicationJar == null ? null : applicationJar.toFile());

        var session = mock(MavenSession.class);
        when(session.getSystemProperties()).thenReturn(systemProperties);
        when(session.getUserProperties()).thenReturn(userProperties);

        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getToImage()).thenReturn(jibToImage);

        var mojo = new BuildpackMojo(project, session, jibConfigurationService, runner);
        mojo.packExecutable = BuildpackMojo.DEFAULT_PACK_EXECUTABLE;
        return mojo;
    }

    private static final class RecordingRunner implements BuildpackMojo.BuildpackCommandRunner {
        private final int exitCode;
        private String verifiedExecutable;
        private File verifiedWorkingDirectory;
        private final java.util.ArrayList<List<String>> commands = new java.util.ArrayList<>();

        private RecordingRunner(int exitCode) {
            this.exitCode = exitCode;
        }

        @Override
        public void verifyPack(String executable, File workingDirectory, Consumer<String> output) {
            this.verifiedExecutable = executable;
            this.verifiedWorkingDirectory = workingDirectory;
        }

        @Override
        public int run(List<String> command, File workingDirectory, Consumer<String> output) {
            commands.add(command);
            return exitCode;
        }
    }

    private static final class FailingVerifyRunner implements BuildpackMojo.BuildpackCommandRunner {
        @Override
        public void verifyPack(String executable, File workingDirectory, Consumer<String> output) throws IOException {
            throw new IOException("missing");
        }

        @Override
        public int run(List<String> command, File workingDirectory, Consumer<String> output) {
            throw new AssertionError("pack build should not run");
        }
    }

    private static final class InterruptedVerifyRunner implements BuildpackMojo.BuildpackCommandRunner {
        @Override
        public void verifyPack(String executable, File workingDirectory, Consumer<String> output) throws InterruptedException {
            throw new InterruptedException("stop");
        }

        @Override
        public int run(List<String> command, File workingDirectory, Consumer<String> output) {
            throw new AssertionError("pack build should not run");
        }
    }

    private static final class FailingRunRunner implements BuildpackMojo.BuildpackCommandRunner {
        private final Exception exception;

        private FailingRunRunner(Exception exception) {
            this.exception = exception;
        }

        @Override
        public void verifyPack(String executable, File workingDirectory, Consumer<String> output) {
        }

        @Override
        public int run(List<String> command, File workingDirectory, Consumer<String> output) throws IOException, InterruptedException {
            if (exception instanceof IOException ioException) {
                throw ioException;
            }
            if (exception instanceof InterruptedException interruptedException) {
                throw interruptedException;
            }
            throw new AssertionError(exception);
        }
    }
}
