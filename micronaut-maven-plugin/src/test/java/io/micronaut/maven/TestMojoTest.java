package io.micronaut.maven;

import io.micronaut.maven.dev.ControlPanel;
import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.Parameter;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.classworlds.ClassWorld;
import org.codehaus.plexus.component.configurator.BasicComponentConfigurator;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class TestMojoTest {

    @Test
    void mapsSurefireTestToPatterns() {
        assertEquals(List.of(), TestMojo.surefireFilter(null));
        assertEquals(List.of(), TestMojo.surefireFilter(" "));
        assertEquals(List.of("CalculatorTest"), TestMojo.surefireFilter("CalculatorTest"));
        assertEquals(List.of("CalculatorTest.adds"), TestMojo.surefireFilter("CalculatorTest#adds"));
        assertEquals(List.of("CalculatorTest.adds", "CalculatorTest.subtracts"), TestMojo.surefireFilter("CalculatorTest#adds+subtracts"));
        assertEquals(List.of("CalculatorTest.add*"), TestMojo.surefireFilter("CalculatorTest#add*"));
        assertEquals(List.of("com.example.*Test", "OtherTest"), TestMojo.surefireFilter("com.example.*Test, OtherTest"));
        assertEquals(List.of("com.example.FooTest"), TestMojo.surefireFilter("**/com/example/FooTest.java"));
        assertEquals(List.of("FooTest"), TestMojo.surefireFilter("**/FooTest.class"));
        assertEquals(List.of("com.example.FooTest"), TestMojo.surefireFilter("**\\com\\example\\FooTest.java"));
        assertEquals(List.of("*.adds"), TestMojo.surefireFilter("#adds"));
        assertEquals(List.of("!SlowTest", "%regex[.*Test]"), TestMojo.surefireFilter("!SlowTest,%regex[.*Test]"));
    }

    @Test
    void aCustomRunnerLeavesTheJUnitPlatformLauncherOut() throws Exception {
        assertFalse(launcherArtifactIds("custom").contains("junit-platform-launcher"));
    }

    @Test
    void theJUnitPlatformRunnerAddsItsLauncher() throws Exception {
        assertTrue(launcherArtifactIds("junit-platform").contains("junit-platform-launcher"));
    }

    @Test
    void theManifestOfTheTestsHasNoControlPanelAndNoServerSetting(@TempDir Path directory) throws Exception {
        MavenProject project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId("app");
        project.setVersion("1.0");
        project.setFile(directory.resolve("pom.xml").toFile());
        project.getBuild().setDirectory(directory.resolve("target").toString());
        project.getBuild().setOutputDirectory(directory.resolve("target/classes").toString());
        project.getBuild().setTestOutputDirectory(directory.resolve("target/test-classes").toString());
        DependencyManagement management = new DependencyManagement();
        Dependency platform = new Dependency();
        platform.setGroupId(ControlPanel.PLATFORM_GROUP);
        platform.setArtifactId("micronaut-parent");
        platform.setVersion(ControlPanel.MINIMUM_PLATFORM_VERSION);
        management.addDependency(platform);
        project.getModel().setDependencyManagement(management);
        MavenSession session = mock(MavenSession.class);
        MavenSession runnableSession = mock(MavenSession.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.clone()).thenReturn(runnableSession);
        when(session.getRepositorySession()).thenReturn(mock(RepositorySystemSession.class));
        when(session.getAllProjects()).thenReturn(List.of(project));
        when(runnableSession.getCurrentProject()).thenReturn(project);
        when(runnableSession.getSystemProperties()).thenReturn(new Properties());
        when(runnableSession.getUserProperties()).thenReturn(new Properties());
        TestMojo mojo = new TestMojo(session, mock(ToolchainManager.class), mock(CompilerService.class),
            mock(ExecutorService.class), mock(RepositorySystem.class), mock(ComponentConfigurator.class));
        DependencyResolutionService resolution = mock(DependencyResolutionService.class);
        set(DevMojo.class, mojo, "runnableProject", project);
        set(DevMojo.class, mojo, "mojoExecution", mock(MojoExecution.class));
        set(DevMojo.class, mojo, "dependencyResolutionService", resolution);
        // what the goal shares with mn:dev, where these turn both on
        set(DevMojo.class, mojo, "controlPanel", true);
        set(DevMojo.class, mojo, "bindLoopback", true);
        set(DevMojo.class, mojo, "strategy", "auto");
        set(DevMojo.class, mojo, "compile", "embedded");
        set(TestMojo.class, mojo, "runner", "junit-platform");
        set(TestMojo.class, mojo, "selection", "affected");
        set(TestMojo.class, mojo, "reportPath", "/report");
        Path manifestDirectory = mojo.manifestDirectory();
        // the settings of an mn:dev running beside these tests
        Path devResources = directory.resolve("target/micronaut-dev").resolve(ControlPanel.RESOURCES_DIRECTORY_NAME);
        ControlPanel.writeResources(devResources, List.of());

        DevManifest manifest = mojo.buildManifest(manifestDirectory);

        assertFalse(manifest.entries().containsKey(DevManifest.BIND_LOOPBACK));
        assertEquals("junit-platform", manifest.entries().get("micronaut.dev.test.runner"));
        verifyNoInteractions(resolution);
        assertFalse(Files.exists(manifestDirectory.resolve(ControlPanel.RESOURCES_DIRECTORY_NAME)));
        assertTrue(Files.isRegularFile(devResources.resolve(ControlPanel.PROPERTIES_RESOURCE)));
    }

    @Test
    void aSettingOfThisGoalResolvedOnlyInTheReactorRootDoesNotRemain() throws Exception {
        MavenProject app = new MavenProject();
        app.setFile(new File("app/pom.xml").getAbsoluteFile());
        app.getProperties().setProperty("mn.test.selection", "all");
        MavenSession session = mock(MavenSession.class);
        MavenSession applicationSession = mock(MavenSession.class);
        when(session.clone()).thenReturn(applicationSession);
        when(applicationSession.getCurrentProject()).thenReturn(app);
        when(applicationSession.getUserProperties()).thenReturn(new Properties());
        when(applicationSession.getSystemProperties()).thenReturn(new Properties());
        MojoExecution execution = mock(MojoExecution.class);
        when(execution.getMojoDescriptor()).thenReturn(descriptor());
        TestMojo mojo = new TestMojo(session, mock(ToolchainManager.class), mock(CompilerService.class),
            mock(ExecutorService.class), mock(RepositorySystem.class), new BasicComponentConfigurator());
        set(DevMojo.class, mojo, "mojoExecution", execution);
        set(DevMojo.class, mojo, "runnableProject", app);
        // as Maven configured the goal from the root, an aggregator the application does not inherit from
        set(TestMojo.class, mojo, "filter", "OnlyInTheRoot");
        set(TestMojo.class, mojo, "selection", "affected");
        set(DevMojo.class, mojo, "retain", "example.OnlyInTheRoot");

        var apply = DevMojo.class.getDeclaredMethod("applyRunnableProjectConfiguration");
        apply.setAccessible(true);
        apply.invoke(mojo);

        assertNull(get(TestMojo.class, mojo, "filter"));
        assertNull(get(DevMojo.class, mojo, "retain"));
        assertEquals("all", get(TestMojo.class, mojo, "selection"), "the application's own value");
    }

    @Test
    void anUnknownSelectionFailsTheGoal() throws Exception {
        TestMojo mojo = new TestMojo(mock(MavenSession.class), mock(ToolchainManager.class), mock(CompilerService.class),
            mock(ExecutorService.class), mock(RepositorySystem.class), mock(ComponentConfigurator.class));
        set(DevMojo.class, mojo, "strategy", "auto");
        set(DevMojo.class, mojo, "compile", "embedded");
        set(TestMojo.class, mojo, "selection", "affectd");

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::validateSettings);

        assertEquals("Unknown value 'affectd' for mn.test.selection: the valid values are affected, all", failure.getMessage());

        set(TestMojo.class, mojo, "selection", "ALL");
        mojo.validateSettings();
        assertEquals("all", get(TestMojo.class, mojo, "selection"));
    }

    private static MojoDescriptor descriptor() throws Exception {
        MojoDescriptor descriptor = new MojoDescriptor();
        PluginDescriptor plugin = new PluginDescriptor();
        plugin.setClassRealm(new ClassWorld("test", TestMojoTest.class.getClassLoader()).getClassRealm("test"));
        descriptor.setPluginDescriptor(plugin);
        XmlPlexusConfiguration configuration = new XmlPlexusConfiguration("configuration");
        for (String name : List.of("filter", "selection", "retain")) {
            Parameter parameter = new Parameter();
            parameter.setName(name);
            parameter.setType(String.class.getName());
            parameter.setEditable(true);
            descriptor.addParameter(parameter);
        }
        configuration.addChild(expression("filter", "${mn.test.filter}"));
        configuration.addChild(expression("selection", "${mn.test.selection}"));
        configuration.addChild(expression("retain", "${mn.dev.retain}"));
        descriptor.setMojoConfiguration(configuration);
        return descriptor;
    }

    private static XmlPlexusConfiguration expression(String name, String expression) {
        XmlPlexusConfiguration child = new XmlPlexusConfiguration(name);
        child.setValue(expression);
        return child;
    }

    private static Object get(Class<?> type, Object target, String name) throws Exception {
        Field field = type.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(target);
    }

    private static List<String> launcherArtifactIds(String runner) throws Exception {
        TestMojo mojo = new TestMojo(mock(MavenSession.class), mock(ToolchainManager.class), mock(CompilerService.class),
            mock(ExecutorService.class), mock(RepositorySystem.class), mock(ComponentConfigurator.class));
        set(TestMojo.class, mojo, "runner", runner);
        set(DevMojo.class, mojo, "runnableProject", new MavenProject());
        return mojo.launcherArtifacts().stream().map(Artifact::getArtifactId).toList();
    }

    private static void set(Class<?> type, Object target, String name, Object value) throws Exception {
        Field field = type.getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
    }
}
