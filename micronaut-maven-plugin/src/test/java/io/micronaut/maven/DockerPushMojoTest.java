package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class DockerPushMojoTest {

    @ParameterizedTest
    @CsvSource({
        "jkube-k8s",
        "jkube-oc"
    })
    void rejectsJkubePackagingsForDirectDockerPushExecution(String packaging, @TempDir Path tempDir) {
        var project = mockProject(packaging, tempDir);
        var session = mockSession(project);

        var mojo = new DockerPushMojo(
            project,
            mock(JibConfigurationService.class),
            null,
            null,
            session,
            mock(MojoExecution.class)
        );

        var ex = assertThrows(MojoFailureException.class, mojo::execute);

        assertEquals("The <packaging> must be set to either [docker] or [docker-native]", ex.getMessage());
    }

    private static MavenProject mockProject(String packaging, Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getPackaging()).thenReturn(packaging);
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
