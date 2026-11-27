package io.micronaut.maven.classpath;

import io.micronaut.aot.bytecode.ClassPathTransform;
import io.micronaut.maven.services.DependencyResolutionService;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ClassPathTransformServiceTest {

    private static final ClassPathTransformOptions DESUGAR = new ClassPathTransformOptions(true, true, "1.0");

    @Test
    void doesNotRewriteWithoutAStep(@TempDir Path tempDir) throws Exception {
        var service = new ClassPathTransformService(null, null, null);

        assertSame(RewrittenClassPath.none(), service.rewrite(project(tempDir), new ClassPathTransformOptions(false, false, "1.0"),
            tempDir, "docker", mock(Log.class)));
    }

    @Test
    void rewritesTheThirdPartyJarsOfTheImageInAForkedJvm(@TempDir Path tempDir) throws Exception {
        Path jar = codeSource(StringUtils.class);
        Path classes = Files.createDirectories(tempDir.resolve("classes"));
        var project = project(tempDir,
            artifact("org.apache.commons", "commons-lang3", Artifact.SCOPE_COMPILE, jar.toFile()),
            artifact("example", "test-only", Artifact.SCOPE_TEST, tempDir.resolve("test.jar").toFile()),
            artifact("example", "missing", Artifact.SCOPE_RUNTIME, tempDir.resolve("missing.jar").toFile()),
            artifact("example", "no-file", Artifact.SCOPE_RUNTIME, null),
            artifact("example", "classes", Artifact.SCOPE_RUNTIME, classes.toFile()));
        var log = mock(Log.class);

        try (var rewritten = service(List.of(codeSource(ClassPathTransform.class))).rewrite(project, DESUGAR, classes, "docker", log)) {
            Path copy = rewritten.packaged(jar);
            assertNotEquals(jar.toAbsolutePath(), copy);
            assertEquals(jar.getFileName(), copy.getFileName());
            assertTrue(copy.startsWith(tempDir.resolve("target/class-path-transform/docker")));
        }
        List<String> request = Files.readAllLines(tempDir.resolve("target/class-path-transform/docker/request.txt"));
        assertEquals(List.of(
            "output\t" + tempDir.resolve("target/class-path-transform/docker").toAbsolutePath(),
            "desugar\ttrue",
            "strip\ttrue",
            "entry\t" + classes.toAbsolutePath(),
            "entry\t" + jar.toAbsolutePath(),
            "stripped\t" + jar.toAbsolutePath()
        ), request);
        verify(log, atLeastOnce()).info(any(CharSequence.class));
    }

    @Test
    void failsWithTheOutputOfAFailedTransform(@TempDir Path tempDir) throws Exception {
        // Without micronaut-aot-bytecode on its class path, the forked JVM cannot run the transform
        var service = service(List.of());

        var e = assertThrows(MojoExecutionException.class,
            () -> service.rewrite(project(tempDir), DESUGAR, tempDir.resolve("classes"), "docker", mock(Log.class)));

        assertTrue(e.getMessage().startsWith("The class path transform failed with exit code"), e.getMessage());
        assertTrue(e.getMessage().contains("ClassPathTransform"), e.getMessage());
    }

    @Test
    void failsWhenMicronautAotBytecodeCannotBeResolved(@TempDir Path tempDir) throws Exception {
        var resolution = mock(DependencyResolutionService.class);
        when(resolution.artifactResultsFor(any(), anyBoolean())).thenThrow(mock(DependencyResolutionException.class));
        var service = new ClassPathTransformService(session(), resolution, mock(ToolchainManager.class));

        var e = assertThrows(MojoExecutionException.class,
            () -> service.rewrite(project(tempDir), DESUGAR, tempDir.resolve("classes"), "docker", mock(Log.class)));

        assertEquals("Unable to resolve io.micronaut.aot:micronaut-aot-bytecode:1.0", e.getMessage());
    }

    private static ClassPathTransformService service(List<Path> launcherClassPath) throws Exception {
        var resolution = mock(DependencyResolutionService.class);
        List<ArtifactResult> results = launcherClassPath.stream()
            .map(jar -> new ArtifactResult(new ArtifactRequest())
                .setArtifact(new org.eclipse.aether.artifact.DefaultArtifact("example:launcher:1.0").setFile(jar.toFile())))
            .toList();
        when(resolution.artifactResultsFor(any(), anyBoolean())).thenReturn(results);
        return new ClassPathTransformService(session(), resolution, mock(ToolchainManager.class));
    }

    private static MavenSession session() {
        var session = mock(MavenSession.class);
        var reactorProject = mock(MavenProject.class);
        when(reactorProject.getGroupId()).thenReturn("example");
        when(reactorProject.getArtifactId()).thenReturn("classes");
        when(session.getProjects()).thenReturn(List.of(reactorProject));
        return session;
    }

    private static MavenProject project(Path tempDir, Artifact... artifacts) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getBuild()).thenReturn(build);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        when(project.getArtifacts()).thenReturn(new LinkedHashSet<>(List.of(artifacts)));
        return project;
    }

    private static Artifact artifact(String groupId, String artifactId, String scope, File file) {
        var artifact = new DefaultArtifact(groupId, artifactId, "1.0", scope, "jar", null, new DefaultArtifactHandler("jar"));
        artifact.setFile(file);
        return artifact;
    }

    private static Path codeSource(Class<?> type) throws Exception {
        return Path.of(type.getProtectionDomain().getCodeSource().getLocation().toURI());
    }
}
