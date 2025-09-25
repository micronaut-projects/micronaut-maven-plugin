package io.micronaut.maven.testresources;

import io.micronaut.maven.MojoUtils;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class DefaultServerFactoryTest {

    @Test
    void itPassesSystemProperties() {
        try (MockedStatic<MojoUtils> mojoUtils = mockStatic(MojoUtils.class)) {
            var serverFactory = new DefaultServerFactory(null, null, null, null, null, false, false, Map.of("foo", "bar"));
            mojoUtils.when(() -> MojoUtils.findJavaExecutable(any(), any())).thenReturn("java");
            var cliArguments = serverFactory.computeCliArguments(createProcessParameters());
            assertTrue(cliArguments.contains("-Dfoo=bar"));
        }
    }

    @Test
    void itHandlesMisconfiguredToolchain() {
        // Test that DefaultServerFactory doesn't throw IllegalStateException when toolchain is misconfigured
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        MavenSession mavenSession = mock(MavenSession.class);
        Toolchain toolchain = mock(Toolchain.class);
        
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(toolchain);
        when(toolchain.findTool("java")).thenReturn(null); // Simulates misconfigured toolchain
        
        var serverFactory = new DefaultServerFactory(null, toolchainManager, mavenSession, null, null, false, false, Map.of());
        
        // This should not throw IllegalStateException anymore
        assertDoesNotThrow(() -> {
            var cliArguments = serverFactory.computeCliArguments(createProcessParameters());
            assertNotNull(cliArguments);
            assertFalse(cliArguments.isEmpty());
            // First argument should be the Java executable (not null)
            assertNotNull(cliArguments.get(0));
        });
    }

    private ServerUtils.ProcessParameters createProcessParameters() {
        var processParameters = mock(ServerUtils.ProcessParameters.class);
        when(processParameters.getMainClass()).thenReturn("com.example.MyApp");
        return processParameters;
    }
}
