package io.micronaut.maven.services;

import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ExecutorServiceTest {

    @TempDir
    Path tempDir;

    @Test
    void resolveOriginalPomReturnsOriginalPomWhenFileIsPomXml() throws IOException {
        File pomFile = tempDir.resolve("pom.xml").toFile();
        Files.writeString(pomFile.toPath(), "<project/>");

        MavenProject project = mockProject(pomFile, tempDir.resolve("target").toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(pomFile, result);
    }

    @Test
    void resolveOriginalPomFallsBackToOriginalWhenFileIsInTargetDirectory() throws IOException {
        // Simulate a processed POM in the target directory (e.g., from flatten-maven-plugin)
        Path targetDir = tempDir.resolve("target");
        Files.createDirectories(targetDir);
        File processedPom = targetDir.resolve("foo-1.0.0-SNAPSHOT.pom").toFile();
        Files.writeString(processedPom.toPath(), "<project/>");

        // The original pom.xml should exist in the project basedir
        File originalPom = tempDir.resolve("pom.xml").toFile();
        Files.writeString(originalPom.toPath(), "<project/>");

        MavenProject project = mockProject(processedPom, targetDir.toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(originalPom, result);
    }

    @Test
    void resolveOriginalPomFallsBackToProjectFileWhenNoPomXmlFound() throws IOException {
        // Simulate a processed POM in an arbitrary directory with no pom.xml nearby
        Path arbitraryDir = tempDir.resolve("arbitrary");
        Files.createDirectories(arbitraryDir);
        File processedPom = arbitraryDir.resolve("foo-1.0.0-SNAPSHOT.pom").toFile();
        Files.writeString(processedPom.toPath(), "<project/>");

        MavenProject project = mockProject(processedPom, arbitraryDir.toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(processedPom, result);
    }

    @Test
    void resolveOriginalPomReturnsNullWhenProjectFileIsNull() {
        MavenProject project = mockProject(null, "/some/path/target");

        File result = ExecutorService.resolveOriginalPom(project);
        assertNull(result);
    }

    @Test
    void resolveOriginalPomHandlesFlattenedPomInBasedir() throws IOException {
        // Simulate flatten-maven-plugin with default output (.flattened-pom.xml in basedir)
        File flattenedPom = tempDir.resolve(".flattened-pom.xml").toFile();
        Files.writeString(flattenedPom.toPath(), "<project/>");

        File originalPom = tempDir.resolve("pom.xml").toFile();
        Files.writeString(originalPom.toPath(), "<project/>");

        MavenProject project = mockProject(flattenedPom, tempDir.resolve("target").toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(originalPom, result);
    }

    @Test
    void resolveOriginalPomPreservesLegitimateNonStandardPomFilename() throws IOException {
        // Simulate a legitimate alternate POM filename provided via maven -f
        File alternatePom = tempDir.resolve("parent.xml").toFile();
        Files.writeString(alternatePom.toPath(), "<project/>");

        // Even if a pom.xml exists, the alternate POM should not be overridden
        File pomXml = tempDir.resolve("pom.xml").toFile();
        Files.writeString(pomXml.toPath(), "<project/>");

        MavenProject project = mockProject(alternatePom, tempDir.resolve("target").toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(alternatePom, result);
    }

    @Test
    void resolveOriginalPomHandlesDependencyReducedPom() throws IOException {
        // Simulate maven-shade-plugin with dependency-reduced-pom.xml in basedir
        File reducedPom = tempDir.resolve("dependency-reduced-pom.xml").toFile();
        Files.writeString(reducedPom.toPath(), "<project/>");

        File originalPom = tempDir.resolve("pom.xml").toFile();
        Files.writeString(originalPom.toPath(), "<project/>");

        MavenProject project = mockProject(reducedPom, tempDir.resolve("target").toString());

        File result = ExecutorService.resolveOriginalPom(project);
        assertEquals(originalPom, result);
    }

    private static MavenProject mockProject(File pomFile, String buildDirectory) {
        Build build = mock(Build.class);
        when(build.getDirectory()).thenReturn(buildDirectory);
        MavenProject project = mock(MavenProject.class);
        when(project.getFile()).thenReturn(pomFile);
        when(project.getBuild()).thenReturn(build);
        return project;
    }
}
