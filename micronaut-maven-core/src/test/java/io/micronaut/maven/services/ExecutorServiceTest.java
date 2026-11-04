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
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ExecutorServiceTest {

    @Test
    void invokeGoalsAsTheSessionBuildsCopiesItsPropertiesAndProfiles(@TempDir Path tempDir) throws Exception {
        MavenProject root = new MavenProject();
        root.setFile(tempDir.resolve("pom.xml").toFile());
        MavenProject app = new MavenProject();
        app.setGroupId("example");
        app.setArtifactId("app");

        Invoker invoker = mock(Invoker.class);
        MavenSession session = newSession(tempDir, false, null);
        Properties userProperties = new Properties();
        userProperties.setProperty("revision", "1.2.3");
        // the session's test resources setting does not reach the compilation, the override does
        userProperties.setProperty("micronaut.test.resources.enabled", "true");
        when(session.getRequest().getUserProperties()).thenReturn(userProperties);
        when(session.getRequest().getActiveProfiles()).thenReturn(List.of("foo", "bar"));
        when(session.getRequest().getInactiveProfiles()).thenReturn(List.of("baz"));
        when(invoker.execute(any(InvocationRequest.class))).thenReturn(mock(InvocationResult.class));

        new ExecutorService(root, session, mock(BuildPluginManager.class), invoker).invokeGoals(root, app, "compile");

        ArgumentCaptor<InvocationRequest> requestCaptor = ArgumentCaptor.forClass(InvocationRequest.class);
        verify(invoker).execute(requestCaptor.capture());
        InvocationRequest request = requestCaptor.getValue();
        assertEquals(tempDir.resolve("pom.xml").toFile(), request.getPomFile());
        assertEquals("1.2.3", request.getProperties().getProperty("revision"));
        assertEquals("false", request.getProperties().getProperty("micronaut.test.resources.enabled"));
        assertEquals(List.of("foo", "bar", "!baz"), request.getProfiles());
        assertEquals(List.of("example:app"), request.getProjects());
        assertTrue(request.isAlsoMake());
        assertEquals(List.of("compile"), request.getArgs());
        assertEquals("true", userProperties.getProperty("micronaut.test.resources.enabled"), "the session's own properties are left alone");
    }

    @Test
    void invokeGoalsWithoutASelectedProjectLeavesTheModelAsItWas(@TempDir Path tempDir) throws Exception {
        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());

        Invoker invoker = mock(Invoker.class);
        MavenSession session = newSession(tempDir, false, null);
        Properties userProperties = new Properties();
        userProperties.setProperty("revision", "1.2.3");
        when(session.getRequest().getUserProperties()).thenReturn(userProperties);
        when(session.getRequest().getActiveProfiles()).thenReturn(List.of("foo"));
        when(invoker.execute(any(InvocationRequest.class))).thenReturn(mock(InvocationResult.class));

        new ExecutorService(project, session, mock(BuildPluginManager.class), invoker).invokeGoals(project, "compile");

        ArgumentCaptor<InvocationRequest> requestCaptor = ArgumentCaptor.forClass(InvocationRequest.class);
        verify(invoker).execute(requestCaptor.capture());
        InvocationRequest request = requestCaptor.getValue();
        assertEquals("false", request.getProperties().getProperty("micronaut.test.resources.enabled"));
        assertNull(request.getProperties().getProperty("revision"));
        assertTrue(request.getProfiles() == null || request.getProfiles().isEmpty());
        assertTrue(request.getProjects() == null || request.getProjects().isEmpty());
    }

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
        Plugin plugin = plugin("com.google.cloud.tools", "jib-maven-plugin", "3.5.1");
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
        Plugin directPlugin = plugin("com.google.cloud.tools", "jib-maven-plugin", "3.5.1");
        build.addPlugin(directPlugin);
        PluginManagement pluginManagement = new PluginManagement();
        Plugin managedPlugin = plugin("com.google.cloud.tools", "jib-maven-plugin", "3.5.0");
        pluginManagement.addPlugin(managedPlugin);
        build.setPluginManagement(pluginManagement);
        project.setBuild(build);

        Plugin result = ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin");

        assertSame(directPlugin, result);
    }

    @Test
    void resolveBuildPluginReturnsNullWithoutPluginManagement() {
        MavenProject project = new MavenProject();

        assertNull(ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin"));

        project.setBuild(new Build());

        assertNull(ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin"));
    }

    @Test
    void resolveBuildPluginIgnoresNullIncompleteAndNonMatchingManagedPlugins() {
        MavenProject project = new MavenProject();
        Build build = new Build();
        PluginManagement pluginManagement = new PluginManagement();
        pluginManagement.getPlugins().add(null);
        pluginManagement.addPlugin(plugin("com.google.cloud.tools", null, "3.5.1"));
        pluginManagement.addPlugin(plugin("com.google.cloud.tools", "other-maven-plugin", null));
        build.setPluginManagement(pluginManagement);
        project.setBuild(build);

        Plugin result = ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin");

        assertNull(result);
    }

    @Test
    void resolveBuildPluginMatchesManagedPluginWithVersionedKey() {
        MavenProject project = new MavenProject();
        Build build = new Build();
        PluginManagement pluginManagement = new PluginManagement();
        Plugin plugin = plugin("com.google.cloud.tools", "jib-maven-plugin", "3.5.1");
        pluginManagement.addPlugin(plugin);
        build.setPluginManagement(pluginManagement);
        project.setBuild(build);

        Plugin result = ExecutorService.resolveBuildPlugin(project, "com.google.cloud.tools:jib-maven-plugin:3.5.1");

        assertSame(plugin, result);
    }

    private static Plugin plugin(String groupId, String artifactId, String version) {
        Plugin plugin = new Plugin();
        plugin.setGroupId(groupId);
        plugin.setArtifactId(artifactId);
        plugin.setVersion(version);
        return plugin;
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
