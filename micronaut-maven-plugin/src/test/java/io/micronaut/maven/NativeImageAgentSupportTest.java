package io.micronaut.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class NativeImageAgentSupportTest {

    @TempDir
    Path tempDir;

    @Test
    void doesNotAddAgentArgumentsWhenAgentIsDisabled() throws MojoExecutionException {
        var arguments = NativeImageAgentSupport.computeJvmArguments(session(new Properties(), new Properties()), project(null), targetDirectory(), List.of());

        assertEquals(List.of(), arguments);
    }

    @Test
    void addsDefaultAgentArgumentsWhenEnabledFromCommandLine() throws MojoExecutionException {
        var userProperties = new Properties();
        userProperties.setProperty("agent", "true");

        var arguments = NativeImageAgentSupport.computeJvmArguments(session(userProperties, new Properties()), project(null), targetDirectory(), List.of());

        assertEquals(2, arguments.size());
        assertEquals("-Dorg.graalvm.nativeimage.imagecode=agent", arguments.get(1));
        assertTrue(arguments.get(0).startsWith("-agentlib:native-image-agent=config-output-dir=" + targetDirectory().getAbsolutePath() + File.separator + "native" + File.separator + "agent-output" + File.separator + "main"));
    }

    @Test
    void rejectsManualNativeImageAgentWhenNativeBuildToolsAgentIsEnabled() {
        var userProperties = new Properties();
        userProperties.setProperty("agent", "true");

        var exception = assertThrows(MojoExecutionException.class, () ->
            NativeImageAgentSupport.computeJvmArguments(session(userProperties, new Properties()), project(null), targetDirectory(), List.of("-agentlib:native-image-agent=config-output-dir=custom"))
        );

        assertTrue(exception.getMessage().contains("mn.jvmArgs"));
    }

    @Test
    void rejectsUnsupportedDirectModeConfiguration() {
        var exception = assertThrows(MojoExecutionException.class, () ->
            NativeImageAgentSupport.computeJvmArguments(session(new Properties(), new Properties()), project(agentConfiguration("direct")), targetDirectory(), List.of())
        );

        assertTrue(exception.getMessage().contains("standard mode"));
    }

    private MavenSession session(Properties userProperties, Properties systemProperties) {
        var session = mock(MavenSession.class);
        when(session.getUserProperties()).thenReturn(userProperties);
        when(session.getSystemProperties()).thenReturn(systemProperties);
        return session;
    }

    private MavenProject project(Xpp3Dom configuration) {
        var project = mock(MavenProject.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        if (configuration != null) {
            var plugin = new Plugin();
            plugin.setGroupId("org.graalvm.buildtools");
            plugin.setArtifactId("native-maven-plugin");
            plugin.setConfiguration(configuration);
            when(project.getPlugin(NativeImageAgentSupport.NATIVE_MAVEN_PLUGIN)).thenReturn(plugin);
        }
        return project;
    }

    private File targetDirectory() {
        return tempDir.resolve("target").toFile();
    }

    private Xpp3Dom agentConfiguration(String mode) {
        var configuration = new Xpp3Dom("configuration");
        var agent = new Xpp3Dom("agent");
        configuration.addChild(agent);

        var enabled = new Xpp3Dom("enabled");
        enabled.setValue("true");
        agent.addChild(enabled);

        var defaultMode = new Xpp3Dom("defaultMode");
        defaultMode.setValue(mode);
        agent.addChild(defaultMode);

        var modes = new Xpp3Dom("modes");
        agent.addChild(modes);
        var direct = new Xpp3Dom("direct");
        direct.setValue("config-output-dir={output_dir}");
        modes.addChild(direct);
        return configuration;
    }
}
