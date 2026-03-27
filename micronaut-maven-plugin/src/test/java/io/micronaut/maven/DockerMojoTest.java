package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DockerMojoTest {

    @Test
    void executesConfiguredJibGoalWithinCurrentBuild(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "buildTar";

        mojo.execute();

        verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildTar");
    }

    @Test
    void rejectsUnknownJibGoal(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "buildDirectory";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals(
            "Unsupported jib.buildGoal 'buildDirectory'. Supported values are: dockerBuild, build, buildTar",
            ex.getMessage()
        );
        verify(executorService, never()).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildDirectory");
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getBuild()).thenReturn(build);
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
