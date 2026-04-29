package io.micronaut.maven.services;

import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.Invoker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ExecutorServiceTest {

    @Test
    void invokeGoalsKeepsInvokerHandlersUnsetWhenSessionIsQuiet(@TempDir Path tempDir) throws Exception {
        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());

        Invoker invoker = mock(Invoker.class);
        MavenSession session = newSession(tempDir, true, null);
        when(invoker.execute(any(InvocationRequest.class))).thenReturn(mock(InvocationResult.class));

        ExecutorService service = new ExecutorService(project, session, mock(BuildPluginManager.class), invoker);
        service.invokeGoals("package");

        ArgumentCaptor<InvocationRequest> requestCaptor = ArgumentCaptor.forClass(InvocationRequest.class);
        verify(invoker).execute(requestCaptor.capture());
        InvocationRequest request = requestCaptor.getValue();
        assertEquals(true, request.isQuiet());
        assertNull(request.getOutputHandler(null));
        assertNull(request.getErrorHandler(null));
    }

    @Test
    void invokeGoalsPreservesNonQuietSessionsAndSettingsFile(@TempDir Path tempDir) throws Exception {
        Path settingsFile = tempDir.resolve("settings.xml");
        Files.writeString(settingsFile, "<settings/>");

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());

        Invoker invoker = mock(Invoker.class);
        MavenSession session = newSession(tempDir, false, settingsFile.toFile());
        when(invoker.execute(any(InvocationRequest.class))).thenReturn(mock(InvocationResult.class));

        ExecutorService service = new ExecutorService(project, session, mock(BuildPluginManager.class), invoker);
        service.invokeGoals("package");

        ArgumentCaptor<InvocationRequest> requestCaptor = ArgumentCaptor.forClass(InvocationRequest.class);
        verify(invoker).execute(requestCaptor.capture());
        InvocationRequest request = requestCaptor.getValue();
        assertEquals(false, request.isQuiet());
        assertEquals(settingsFile.toFile().getAbsolutePath(), request.getUserSettingsFile().getAbsolutePath());
        assertNotNull(request.getOutputHandler(null));
        assertNotNull(request.getErrorHandler(null));
    }

    @Test
    void resolveOriginalPomReturnsPomInsideProjectDirectoryForProcessedPom(@TempDir Path tempDir) {
        Path projectDirectory = tempDir.resolve("project");
        Path buildDirectory = projectDirectory.resolve("target");
        File originalPom = projectDirectory.resolve("pom.xml").toFile();
        try {
            Files.createDirectories(buildDirectory);
            Files.writeString(originalPom.toPath(), "<project/>");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        MavenProject project = new MavenProject();
        org.apache.maven.model.Build build = new org.apache.maven.model.Build();
        build.setDirectory(buildDirectory.toString());
        project.setBuild(build);
        project.setFile(buildDirectory.resolve("flattened-pom.xml").toFile());

        assertEquals(originalPom, ExecutorService.resolveOriginalPom(project));
    }

    @Test
    void resolveOriginalPomKeepsExplicitAlternatePomNames(@TempDir Path tempDir) {
        Path projectDirectory = tempDir.resolve("project");
        Path buildDirectory = projectDirectory.resolve("target");
        MavenProject project = new MavenProject();
        Build build = new Build();
        build.setDirectory(buildDirectory.toString());
        project.setBuild(build);
        File alternatePom = projectDirectory.resolve("custom.xml").toFile();
        project.setFile(alternatePom);

        assertSame(alternatePom, ExecutorService.resolveOriginalPom(project));
    }

    @Test
    void resolveBuildPluginFallsBackToManagedPlugin() {
        MavenProject project = new MavenProject();
        Build build = new Build();
        PluginManagement pluginManagement = new PluginManagement();
        Plugin plugin = new Plugin();
        plugin.setGroupId("com.google.cloud.tools");
        plugin.setArtifactId("jib-maven-plugin");
        plugin.setVersion("3.5.1");
        pluginManagement.addPlugin(plugin);
        build.setPluginManagement(pluginManagement);
        project.setBuild(build);

        Plugin result = ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin");

        assertSame(plugin, result);
    }

    @Test
    void resolveBuildPluginPrefersDirectBuildPluginOverManagedPlugin() {
        MavenProject project = new MavenProject();
        Build build = new Build();
        Plugin directPlugin = new Plugin();
        directPlugin.setGroupId("com.google.cloud.tools");
        directPlugin.setArtifactId("jib-maven-plugin");
        directPlugin.setVersion("3.5.1");
        build.addPlugin(directPlugin);
        PluginManagement pluginManagement = new PluginManagement();
        Plugin managedPlugin = new Plugin();
        managedPlugin.setGroupId("com.google.cloud.tools");
        managedPlugin.setArtifactId("jib-maven-plugin");
        managedPlugin.setVersion("3.5.0");
        pluginManagement.addPlugin(managedPlugin);
        build.setPluginManagement(pluginManagement);
        project.setBuild(build);

        Plugin result = ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin");

        assertSame(directPlugin, result);
    }

    private static MavenSession newSession(Path tempDir, boolean quiet, File settingsFile) {
        MavenSession session = mock(MavenSession.class);
        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        ArtifactRepository localRepository = mock(ArtifactRepository.class);
        when(session.getRequest()).thenReturn(request);
        when(request.getLoggingLevel()).thenReturn(quiet ? MavenExecutionRequest.LOGGING_LEVEL_ERROR : MavenExecutionRequest.LOGGING_LEVEL_INFO);
        when(request.getUserSettingsFile()).thenReturn(settingsFile);
        when(session.getLocalRepository()).thenReturn(localRepository);
        when(localRepository.getBasedir()).thenReturn(tempDir.resolve("repo").toString());
        return session;
    }
}
