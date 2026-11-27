package io.micronaut.maven.classpath;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class TransformClassPathMojoTest {

    private static final ClassPathTransformOptions OPTIONS = new ClassPathTransformOptions(true, false, "1.0");

    @Test
    void doesNothingWithoutAStep(@TempDir Path tempDir) throws Exception {
        var service = mock(ClassPathTransformService.class);
        var project = project(tempDir, null);

        mojo(project, service, false).execute();

        verifyNoInteractions(service);
    }

    @Test
    void replacesTheProjectJarWithItsRewrittenCopy(@TempDir Path tempDir) throws Exception {
        Path jar = Files.writeString(tempDir.resolve("app.jar"), "original");
        Path copy = Files.writeString(tempDir.resolve("copy.jar"), "rewritten");
        var project = project(tempDir, jar);
        var rewritten = new RewrittenClassPath(Map.of(jar.toAbsolutePath(), copy));
        var service = mock(ClassPathTransformService.class);
        when(service.rewrite(eq(project), eq(OPTIONS), eq(jar), eq("shade"), any(Log.class))).thenReturn(rewritten);

        mojo(project, service, true).execute();

        assertEquals("rewritten", Files.readString(jar));
        verify(project).setContextValue(TransformClassPathMojo.CONTEXT_KEY, rewritten);
    }

    @Test
    void rewritesTheClassesDirectoryWithoutAProjectJar(@TempDir Path tempDir) throws Exception {
        Path classes = Files.createDirectories(tempDir.resolve("classes"));
        var project = project(tempDir, tempDir.resolve("missing.jar"));
        var service = mock(ClassPathTransformService.class);
        when(service.rewrite(eq(project), eq(OPTIONS), eq(classes), eq("shade"), any(Log.class)))
            .thenReturn(RewrittenClassPath.none());

        mojo(project, service, true).execute();

        verify(project).setContextValue(TransformClassPathMojo.CONTEXT_KEY, RewrittenClassPath.none());
    }

    @Test
    void failsWhenTheProjectJarCannotBeReplaced(@TempDir Path tempDir) throws Exception {
        Path jar = Files.writeString(tempDir.resolve("app.jar"), "original");
        var project = project(tempDir, jar);
        var service = mock(ClassPathTransformService.class);
        when(service.rewrite(eq(project), eq(OPTIONS), eq(jar), eq("shade"), any(Log.class)))
            .thenReturn(new RewrittenClassPath(Map.of(jar.toAbsolutePath(), tempDir.resolve("missing-copy.jar"))));

        var e = assertThrows(MojoExecutionException.class, () -> mojo(project, service, true).execute());

        assertEquals("Unable to replace " + jar + " with its rewritten copy", e.getMessage());
    }

    private static MavenProject project(Path tempDir, Path jar) {
        var project = mock(MavenProject.class);
        var artifact = mock(Artifact.class);
        var build = mock(Build.class);
        when(project.getArtifact()).thenReturn(artifact);
        when(project.getArtifacts()).thenReturn(Set.of());
        when(artifact.getFile()).thenReturn(jar == null ? null : jar.toFile());
        when(project.getBuild()).thenReturn(build);
        when(build.getOutputDirectory()).thenReturn(tempDir.resolve("classes").toString());
        return project;
    }

    private static TransformClassPathMojo mojo(MavenProject project, ClassPathTransformService service, boolean desugar) {
        var mojo = new TransformClassPathMojo(project, service);
        set(mojo, "desugarLambdas", desugar);
        set(mojo, "version", "1.0");
        return mojo;
    }

    private static void set(TransformClassPathMojo mojo, String name, Object value) {
        try {
            var field = TransformClassPathMojo.class.getDeclaredField(name);
            field.setAccessible(true);
            field.set(mojo, value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }
}
