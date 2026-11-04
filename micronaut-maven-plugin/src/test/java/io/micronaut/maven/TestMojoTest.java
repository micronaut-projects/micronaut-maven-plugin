package io.micronaut.maven;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.Artifact;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

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
