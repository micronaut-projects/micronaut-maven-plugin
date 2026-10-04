package io.micronaut.maven;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.Parameter;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.classworlds.ClassWorld;
import org.codehaus.plexus.component.configurator.BasicComponentConfigurator;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * {@code mn:run} from a reactor root configures itself as the application it selects declares it.
 */
class RunMojoReactorConfigurationTest {

    private static final String ENABLED = "micronaut.test.resources.enabled";

    @TempDir
    Path tempDir;

    @Test
    void anApplicationThatEnablesTestResourcesItselfRunsWithThem() throws Exception {
        MavenProject root = project("root", Map.of());
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(dependencies("resolver"));
        MavenProject app = project("app", Map.of(ENABLED, "true"));
        app.getBuild().addPlugin(thisPlugin(configuration, null));
        // as Maven configured the goal from the root, which does not enable Test Resources
        RunMojo mojo = mojo(root, app, new Properties());
        set(mojo, "testResourcesEnabled", false);
        set(mojo, "buildDirectory", new File(root.getBuild().getDirectory()));

        mojo.initialize();

        assertTrue((Boolean) get(mojo, "testResourcesEnabled"));
        assertEquals(new File(app.getBuild().getDirectory()), get(mojo, "buildDirectory"));
        List<?> testResourcesDependencies = (List<?>) get(mojo, "testResourcesDependencies");
        assertEquals(1, testResourcesDependencies.size());
        assertEquals("resolver", ((Dependency) testResourcesDependencies.get(0)).getArtifactId());
    }

    @Test
    void anApplicationThatDisablesTestResourcesRunsWithoutThemWhenTheRootEnablesThem() throws Exception {
        MavenProject root = project("root", Map.of(ENABLED, "true"));
        MavenProject app = project("app", Map.of(ENABLED, "false"));
        app.getBuild().addPlugin(thisPlugin(null, null));
        // as Maven configured the goal from the root, which enables Test Resources
        RunMojo mojo = mojo(root, app, new Properties());
        set(mojo, "testResourcesEnabled", true);

        mojo.initialize();

        assertFalse((Boolean) get(mojo, "testResourcesEnabled"));
    }

    @Test
    void theCommandLineStillWins() throws Exception {
        MavenProject root = project("root", Map.of());
        MavenProject app = project("app", Map.of(ENABLED, "false"));
        app.getBuild().addPlugin(thisPlugin(null, null));
        var userProperties = new Properties();
        userProperties.setProperty(ENABLED, "true");
        RunMojo mojo = mojo(root, app, userProperties);

        mojo.initialize();

        assertTrue((Boolean) get(mojo, "testResourcesEnabled"));
    }

    @Test
    void theDefaultCliExecutionIsMergedOverThePluginConfigurationAndOtherGoalsSettingsAreLeftOut() {
        Xpp3Dom pluginConfiguration = new Xpp3Dom("configuration");
        pluginConfiguration.addChild(value("testResourcesEnabled", "false"));
        pluginConfiguration.addChild(value("shared", "true"));
        pluginConfiguration.addChild(value("appArguments", "--plugin"));
        Xpp3Dom executionConfiguration = new Xpp3Dom("configuration");
        executionConfiguration.addChild(value("testResourcesEnabled", "true"));
        MavenProject app = project("app", Map.of());
        app.getBuild().addPlugin(thisPlugin(pluginConfiguration, executionConfiguration));

        Xpp3Dom configuration = RunMojo.runnableProjectConfiguration(app, descriptor());

        assertEquals("true", configuration.getChild("testResourcesEnabled").getValue());
        assertEquals("true", configuration.getChild("shared").getValue());
        // appArguments is not a parameter of the descriptor here: another goal's setting
        assertNull(configuration.getChild("appArguments"));
    }

    private RunMojo mojo(MavenProject root, MavenProject app, Properties userProperties) throws Exception {
        MavenSession session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(root);
        when(session.getTopLevelProject()).thenReturn(root);
        when(session.getProjects()).thenReturn(List.of(root, app));
        when(session.getAllProjects()).thenReturn(List.of(root, app));
        when(session.getUserProperties()).thenReturn(userProperties);
        when(session.getSystemProperties()).thenReturn(new Properties());
        MavenSession appSession = mock(MavenSession.class);
        when(appSession.getCurrentProject()).thenReturn(app);
        when(appSession.getTopLevelProject()).thenReturn(root);
        when(appSession.getUserProperties()).thenReturn(userProperties);
        when(appSession.getSystemProperties()).thenReturn(new Properties());
        when(session.clone()).thenReturn(appSession);
        CompilerService compilerService = mock(CompilerService.class);
        when(compilerService.compileProject()).thenReturn(Optional.of(1L));
        RunMojo mojo = new RunMojo(session, mock(BuildPluginManager.class), mock(ProjectBuilder.class),
            mock(ToolchainManager.class), compilerService, mock(ExecutorService.class),
            mock(DependencyResolutionService.class), new BasicComponentConfigurator());
        set(mojo, "mojoExecution", new MojoExecution(descriptor()));
        mojo.setLog(new SystemStreamLog());
        return mojo;
    }

    private static MojoDescriptor descriptor() {
        var descriptor = new MojoDescriptor();
        descriptor.setGoal("run");
        try {
            descriptor.addParameter(parameter("testResourcesEnabled", "boolean", "${" + ENABLED + "}", "false"));
            descriptor.addParameter(parameter("shared", "boolean", "${micronaut.test.resources.shared}", "false"));
            descriptor.addParameter(parameter("buildDirectory", File.class.getName(), null, "${project.build.directory}"));
            descriptor.addParameter(parameter("testResourcesDependencies", List.class.getName(), null, null));
            // the goal's configuration in plugin.xml: the expressions and defaults Maven merges the project's configuration over
            var mojoConfiguration = new XmlPlexusConfiguration("configuration");
            for (Parameter parameter : descriptor.getParameters()) {
                if (parameter.getExpression() != null || parameter.getDefaultValue() != null) {
                    var child = new XmlPlexusConfiguration(parameter.getName());
                    child.setValue(parameter.getExpression());
                    if (parameter.getDefaultValue() != null) {
                        child.setAttribute("default-value", parameter.getDefaultValue());
                    }
                    mojoConfiguration.addChild(child);
                }
            }
            descriptor.setMojoConfiguration(mojoConfiguration);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
        var plugin = new PluginDescriptor();
        plugin.setGroupId("io.micronaut.maven");
        plugin.setArtifactId("micronaut-maven-plugin");
        plugin.setClassRealm(new ClassWorld("test", RunMojoReactorConfigurationTest.class.getClassLoader()).getClassRealm("test"));
        descriptor.setPluginDescriptor(plugin);
        return descriptor;
    }

    private static Parameter parameter(String name, String type, String expression, String defaultValue) {
        var parameter = new Parameter();
        parameter.setName(name);
        parameter.setType(type);
        parameter.setExpression(expression);
        parameter.setDefaultValue(defaultValue);
        parameter.setEditable(true);
        return parameter;
    }

    private MavenProject project(String name, Map<String, String> properties) {
        var project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId(name);
        project.setVersion("0.1");
        File basedir = tempDir.resolve(name).toFile();
        project.setFile(new File(basedir, "pom.xml"));
        project.getBuild().setDirectory(new File(basedir, "target").getAbsolutePath());
        project.getBuild().setOutputDirectory(new File(basedir, "target/classes").getAbsolutePath());
        properties.forEach(project.getProperties()::setProperty);
        return project;
    }

    private static Plugin thisPlugin(Xpp3Dom configuration, Xpp3Dom defaultCliConfiguration) {
        var plugin = new Plugin();
        String[] coordinates = THIS_PLUGIN.split(":");
        plugin.setGroupId(coordinates[0]);
        plugin.setArtifactId(coordinates[1]);
        plugin.setConfiguration(configuration);
        if (defaultCliConfiguration != null) {
            var execution = new PluginExecution();
            execution.setId("default-cli");
            execution.setConfiguration(defaultCliConfiguration);
            plugin.addExecution(execution);
        }
        return plugin;
    }

    private static Xpp3Dom dependencies(String artifactId) {
        Xpp3Dom dependencies = new Xpp3Dom("testResourcesDependencies");
        Xpp3Dom dependency = new Xpp3Dom("dependency");
        dependency.addChild(value("groupId", "example"));
        dependency.addChild(value("artifactId", artifactId));
        dependency.addChild(value("version", "0.1"));
        dependencies.addChild(dependency);
        return dependencies;
    }

    private static Xpp3Dom value(String name, String value) {
        var dom = new Xpp3Dom(name);
        dom.setValue(value);
        return dom;
    }

    private static Field field(String name) throws NoSuchFieldException {
        for (Class<?> type = RunMojo.class; type != null; type = type.getSuperclass()) {
            try {
                Field field = type.getDeclaredField(name);
                field.setAccessible(true);
                return field;
            } catch (NoSuchFieldException e) {
                // the superclass declares it
            }
        }
        throw new NoSuchFieldException(name);
    }

    private static void set(RunMojo mojo, String name, Object value) throws Exception {
        field(name).set(mojo, value);
    }

    private static Object get(RunMojo mojo, String name) throws Exception {
        return field(name).get(mojo);
    }
}
