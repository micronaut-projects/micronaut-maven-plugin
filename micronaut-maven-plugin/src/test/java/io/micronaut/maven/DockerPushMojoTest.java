package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DockerPushMojoTest {

    @Test
    void skipsDockerPushForJibRegistryBuild(@TempDir Path tempDir) throws MojoExecutionException, MojoFailureException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getToImage()).thenReturn(Optional.of("registry.example.com/team/app:1.0"));
        var dockerService = mock(DockerService.class);
        var mojo = new DockerPushMojo(project, jibConfigurationService, null, dockerService, mockSession(project),
            mock(MojoExecution.class));
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "build";

        mojo.execute();

        verify(dockerService, never()).pushImageCmd(anyString());
    }

    @Test
    void rejectsJibRegistryBuildWithoutTargetImage(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        var dockerService = mock(DockerService.class);
        var mojo = new DockerPushMojo(project, jibConfigurationService, null, dockerService, mockSession(project),
            mock(MojoExecution.class));
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "build";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("jib.buildGoal=build requires a configured target image"));
        assertTrue(ex.getMessage().contains("jib.to.image"));
        verify(dockerService, never()).pushImageCmd(anyString());
    }

    @Test
    void rejectsJibTarballDeployBeforeDockerPush(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        var dockerService = mock(DockerService.class);
        var mojo = new DockerPushMojo(project, jibConfigurationService, null, dockerService, mockSession(project),
            mock(MojoExecution.class));
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "buildTar";

        var ex = assertThrows(MojoFailureException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("jib.buildGoal=buildTar is not supported for deploy"));
        assertTrue(ex.getMessage().contains("tar"));
        verify(dockerService, never()).pushImageCmd(anyString());
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        when(project.getPackaging()).thenReturn(Packaging.DOCKER.id());
        when(project.getArtifactId()).thenReturn("app");
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getBuild()).thenReturn(build);
        when(project.getArtifacts()).thenReturn(Set.of());
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
}
