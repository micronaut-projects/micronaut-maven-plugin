package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junitpioneer.jupiter.RestoreSystemProperties;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
class AbstractDockerMojoTest {

    @Test
    void getFromUsesDefaultOracleImageWhenNoOverridesExist(@TempDir Path tempDir) {
        var project = mockProject(tempDir, Set.of());
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());

        var mojo = new TestDockerMojo(project, mockSession(project), jibConfigurationService);

        assertEquals(mojo.defaultBuilderImage(), mojo.from());
    }

    @Test
    void getFromIgnoresBlankJibPomConfiguration(@TempDir Path tempDir) {
        var project = mockProject(tempDir, Set.of());
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of(""));

        var mojo = new TestDockerMojo(project, mockSession(project), jibConfigurationService);

        assertEquals(mojo.defaultBuilderImage(), mojo.from());
    }

    @Test
    void getFromUsesMicronautBaseImageBeforeJibPomConfiguration(@TempDir Path tempDir) {
        var project = mockProject(tempDir, Set.of());
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/graalvm/native-image-community:25-ol9"));

        var mojo = new TestDockerMojo(project, mockSession(project), jibConfigurationService);
        mojo.baseImage = "container-registry.oracle.com/graalvm/native-image:21-ol8";

        assertEquals("container-registry.oracle.com/graalvm/native-image:21-ol8", mojo.from());
    }

    @Test
    void getFromKeepsJibSystemPropertyAsHighestPrecedence(@TempDir Path tempDir) {
        System.setProperty(AbstractDockerMojo.JIB_FROM_IMAGE_PROPERTY, "container-registry.oracle.com/graalvm/native-image-ee:latest");

        var project = mockProject(tempDir, Set.of());
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/graalvm/native-image-community:25-ol9"));

        var mojo = new TestDockerMojo(project, mockSession(project), jibConfigurationService);
        mojo.baseImage = "container-registry.oracle.com/graalvm/native-image:21-ol8";

        assertEquals("container-registry.oracle.com/graalvm/native-image-ee:latest", mojo.from());
    }

    @Test
    void copyDependenciesKeepsFlatLayoutAndAddsReleaseAndSnapshotLayers(@TempDir Path tempDir) throws IOException {
        var releaseJar = Files.writeString(tempDir.resolve("release.jar"), "release");
        var snapshotJar = Files.writeString(tempDir.resolve("snapshot.jar"), "snapshot");
        var testJar = Files.writeString(tempDir.resolve("test.jar"), "test");

        var releaseDependency = mockDependency(Artifact.SCOPE_RUNTIME, false, releaseJar);
        var snapshotDependency = mockDependency(Artifact.SCOPE_COMPILE, true, snapshotJar);
        var testDependency = mockDependency(Artifact.SCOPE_TEST, false, testJar);
        var project = mockProject(tempDir, Set.of(releaseDependency, snapshotDependency, testDependency));

        var mojo = new TestDockerMojo(project, mockSession(project), mock(JibConfigurationService.class));

        mojo.copyDependencies();

        var dependencyDirectory = tempDir.resolve("target").resolve("dependency");
        var flatReleaseJar = dependencyDirectory.resolve("release.jar");
        var flatSnapshotJar = dependencyDirectory.resolve("snapshot.jar");
        var layeredReleaseJar = dependencyDirectory.resolve("release").resolve("release.jar");
        var layeredSnapshotJar = dependencyDirectory.resolve("snapshot").resolve("snapshot.jar");

        assertTrue(Files.exists(flatReleaseJar));
        assertTrue(Files.exists(flatSnapshotJar));
        assertFalse(Files.exists(dependencyDirectory.resolve("test.jar")));

        assertTrue(Files.exists(layeredReleaseJar));
        assertTrue(Files.exists(layeredSnapshotJar));
        assertFalse(Files.exists(dependencyDirectory.resolve("release").resolve("snapshot.jar")));
        assertFalse(Files.exists(dependencyDirectory.resolve("snapshot").resolve("release.jar")));
        assertEquals("release", Files.readString(flatReleaseJar));
        assertEquals("snapshot", Files.readString(flatSnapshotJar));
    }

    private static Artifact mockDependency(String scope, boolean snapshot, Path file) {
        var dependency = mock(Artifact.class);
        when(dependency.getScope()).thenReturn(scope);
        when(dependency.isSnapshot()).thenReturn(snapshot);
        when(dependency.getFile()).thenReturn(file.toFile());
        return dependency;
    }

    private static MavenProject mockProject(Path tempDir, Set<Artifact> dependencies) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getBuild()).thenReturn(build);
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getArtifacts()).thenReturn(dependencies);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        return project;
    }

    private static MavenSession mockSession(MavenProject project) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(new Properties());
        return session;
    }

    private static final class TestDockerMojo extends AbstractDockerMojo {

        private TestDockerMojo(MavenProject mavenProject, MavenSession mavenSession, JibConfigurationService jibConfigurationService) {
            super(
                mavenProject,
                jibConfigurationService,
                mock(ApplicationConfigurationService.class),
                mock(DockerService.class),
                mavenSession,
                mock(MojoExecution.class)
            );
            oracleLinuxVersion = "ol9";
        }

        private String from() {
            return getFrom();
        }

        private String defaultBuilderImage() {
            return DEFAULT_BASE_IMAGE_GRAALVM_BUILD + ":" + graalVmTag(graalVmJvmVersion(), staticNativeImage, oracleLinuxVersion);
        }

        @Override
        public void execute() {
            throw new UnsupportedOperationException("not used in test");
        }
    }
}
