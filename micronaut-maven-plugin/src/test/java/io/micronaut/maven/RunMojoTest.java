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

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class RunMojoTest {

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
}
