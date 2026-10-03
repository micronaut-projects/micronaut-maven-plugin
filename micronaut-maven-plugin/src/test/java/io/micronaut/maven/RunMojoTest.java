package io.micronaut.maven;

import io.micronaut.maven.aot.AbstractAotAnalysisMojo;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Properties;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class RunMojoTest {

    private static final String JMXREMOTE = "-Dcom.sun.management.jmxremote";
    private static final String MAIN_CLASS = "com.example.Application";
    private static final String OUTPUT_DIRECTORY = "/project/target/classes";
    private static final String DEPENDENCIES = "/repo/micronaut-inject.jar";

    @Test
    void runAotPassesExplicitEnabledConfiguration() throws Exception {
        MavenSession mavenSession = mock(MavenSession.class);
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        ExecutorService executorService = mock(ExecutorService.class);
        MavenProject runnableProject = new MavenProject();
        RunMojo mojo = new RunMojo(
            mavenSession,
            mock(BuildPluginManager.class),
            mock(ProjectBuilder.class),
            toolchainManager,
            mock(CompilerService.class),
            executorService,
            mock(DependencyResolutionService.class)
        );
        setField(mojo, "aotEnabled", true);
        setField(mojo, "runnableProject", runnableProject);

        invokeRunAotIfNeeded(mojo);

        ArgumentCaptor<Xpp3Dom> configurationCaptor = ArgumentCaptor.forClass(Xpp3Dom.class);
        verify(executorService).executeGoal(
            eq(runnableProject),
            eq(THIS_PLUGIN),
            eq(AbstractAotAnalysisMojo.NAME),
            configurationCaptor.capture()
        );
        Xpp3Dom enabled = configurationCaptor.getValue().getChild("enabled");
        assertNotNull(enabled);
        assertEquals(Boolean.TRUE.toString(), enabled.getValue());
    }

    @Test
    void runArgumentsDoNotStartTheJmxAgentByDefault() throws Exception {
        RunMojo mojo = runMojoForArguments(new Properties());

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    @Test
    void runArgumentsStartTheJmxAgentWhenTheJvmArgumentsAskForIt() throws Exception {
        RunMojo mojo = runMojoForArguments(new Properties());
        setField(mojo, "jvmArguments", JMXREMOTE);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            JMXREMOTE,
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    @Test
    void runArgumentsStartTheJmxAgentWhenAUserPropertyAsksForIt() throws Exception {
        var userProperties = new Properties();
        // Maven gives a -D without a value the value "true"
        userProperties.setProperty("com.sun.management.jmxremote", "true");
        RunMojo mojo = runMojoForArguments(userProperties);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            JMXREMOTE + "=true",
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    private static RunMojo runMojoForArguments(Properties userProperties) throws Exception {
        MavenSession mavenSession = mock(MavenSession.class);
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        MavenProject runnableProject = new MavenProject();
        runnableProject.getBuild().setOutputDirectory(OUTPUT_DIRECTORY);
        when(mavenSession.getAllProjects()).thenReturn(List.of(runnableProject));
        when(mavenSession.getUserProperties()).thenReturn(userProperties);
        when(mavenSession.getSystemProperties()).thenReturn(new Properties());
        RunMojo mojo = new RunMojo(
            mavenSession,
            mock(BuildPluginManager.class),
            mock(ProjectBuilder.class),
            toolchainManager,
            mock(CompilerService.class),
            mock(ExecutorService.class),
            mock(DependencyResolutionService.class)
        );
        setField(mojo, "runnableProject", runnableProject);
        setField(mojo, "targetDirectory", new File("/project/target"));
        setField(mojo, "classpath", DEPENDENCIES);
        setField(mojo, "mainClass", MAIN_CLASS);
        return mojo;
    }

    private static void setField(RunMojo mojo, String fieldName, Object value) throws Exception {
        Field field = RunMojo.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(mojo, value);
    }

    private static void invokeRunAotIfNeeded(RunMojo mojo) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod("runAotIfNeeded");
        method.setAccessible(true);
        method.invoke(mojo);
    }

    @SuppressWarnings("unchecked")
    private static List<String> invokeBuildRunArguments(RunMojo mojo) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod("buildRunArguments");
        method.setAccessible(true);
        return (List<String>) method.invoke(mojo);
    }
}
