package io.micronaut.maven.classpath;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class RewrittenClassPathTest {

    @Test
    void packagesTheRewrittenCopyOfAnEntryOrTheEntryItself(@TempDir Path tempDir) {
        Path jar = tempDir.resolve("a.jar");
        Path copy = tempDir.resolve("copies/a.jar");
        var rewritten = new RewrittenClassPath(Map.of(jar.toAbsolutePath(), copy));

        assertEquals(copy, rewritten.packaged(jar));
        assertEquals(tempDir.resolve("b.jar"), rewritten.packaged(tempDir.resolve("b.jar")));
        assertEquals(jar, RewrittenClassPath.none().packaged(jar));
    }

    @Test
    void swapsTheDependenciesUntilItIsClosed(@TempDir Path tempDir) {
        File rewrittenJar = tempDir.resolve("a.jar").toFile();
        File keptJar = tempDir.resolve("b.jar").toFile();
        Path copy = tempDir.resolve("copies/a.jar");
        Artifact rewrittenArtifact = artifact("a", rewrittenJar);
        Artifact keptArtifact = artifact("b", keptJar);
        Artifact noFile = artifact("c", null);
        var project = mock(MavenProject.class);
        when(project.getArtifacts()).thenReturn(new LinkedHashSet<>(List.of(rewrittenArtifact, keptArtifact, noFile)));
        var rewritten = new RewrittenClassPath(Map.of(
            rewrittenJar.toPath().toAbsolutePath(), copy,
            keptJar.toPath().toAbsolutePath(), keptJar.toPath().toAbsolutePath()));

        assertSame(rewritten, rewritten.swapDependencies(project));
        assertEquals(copy.toFile(), rewrittenArtifact.getFile());
        assertEquals(keptJar, keptArtifact.getFile());
        assertNull(noFile.getFile());

        // A second swap keeps the original file to restore
        rewritten.swapDependencies(project);
        rewritten.close();

        assertEquals(rewrittenJar, rewrittenArtifact.getFile());
        assertEquals(keptJar, keptArtifact.getFile());
    }

    private static Artifact artifact(String artifactId, File file) {
        var artifact = new DefaultArtifact("example", artifactId, "1.0", Artifact.SCOPE_COMPILE, "jar", null,
            new DefaultArtifactHandler("jar"));
        artifact.setFile(file);
        return artifact;
    }
}
