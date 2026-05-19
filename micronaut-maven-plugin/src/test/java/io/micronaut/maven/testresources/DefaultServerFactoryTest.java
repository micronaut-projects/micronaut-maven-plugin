package io.micronaut.maven.testresources;

import io.micronaut.maven.core.MojoUtils;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

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

    @Test
    void itTracksAndStopsStartedProcesses() throws Exception {
        AtomicBoolean serverStarted = new AtomicBoolean(false);
        Log log = mock(Log.class);
        when(log.isDebugEnabled()).thenReturn(true);

        try (MockedStatic<MojoUtils> mojoUtils = mockStatic(MojoUtils.class)) {
            mojoUtils.when(() -> MojoUtils.findJavaExecutable(any(), any())).thenReturn(System.getProperty("java.home") + "/bin/java");
            var serverFactory = new DefaultServerFactory(log, null, null, serverStarted, "4.0.0", false, false, Map.of());

            serverFactory.startServer(createProcessParameters(SleepingServer.class.getName()));

            assertTrue(serverStarted.get());
            assertFalse(trackedProcesses().isEmpty());

            serverFactory.waitFor(Duration.ofMillis(10));
            DefaultServerFactory.stopAllServers();

            assertTrue(trackedProcesses().isEmpty());
        }
    }

    @Test
    void stopAllServersForciblyDestroysProcessesThatDoNotStop() throws Exception {
        ControllableProcess process = new ControllableProcess(true, false, false);
        trackedProcesses().add(process);

        DefaultServerFactory.stopAllServers();

        assertTrue(process.destroyCalled);
        assertTrue(process.destroyForciblyCalled);
        assertTrue(trackedProcesses().isEmpty());
    }

    @Test
    void stopAllServersRestoresInterruptStatus() throws Exception {
        ControllableProcess process = new ControllableProcess(true, true, true);
        trackedProcesses().add(process);

        DefaultServerFactory.stopAllServers();

        assertTrue(process.destroyCalled);
        assertTrue(process.destroyForciblyCalled);
        assertTrue(Thread.interrupted());
        assertTrue(trackedProcesses().isEmpty());
    }

    private ServerUtils.ProcessParameters createProcessParameters() {
        return createProcessParameters("com.example.MyApp");
    }

    private ServerUtils.ProcessParameters createProcessParameters(String mainClass) {
        var processParameters = mock(ServerUtils.ProcessParameters.class);
        when(processParameters.getJvmArguments()).thenReturn(List.of());
        when(processParameters.getSystemProperties()).thenReturn(Map.of());
        when(processParameters.getClasspath()).thenReturn(Arrays.stream(System.getProperty("java.class.path").split(File.pathSeparator))
                .map(File::new)
                .toList());
        when(processParameters.getMainClass()).thenReturn(mainClass);
        when(processParameters.getArguments()).thenReturn(List.of());
        return processParameters;
    }

    @SuppressWarnings("unchecked")
    private static Set<Process> trackedProcesses() throws Exception {
        Field processes = DefaultServerFactory.class.getDeclaredField("PROCESSES");
        processes.setAccessible(true);
        return (Set<Process>) processes.get(null);
    }

    public static final class SleepingServer {

        public static void main(String[] args) throws Exception {
            Thread.sleep(TimeUnit.MINUTES.toMillis(1));
        }
    }

    private static final class ControllableProcess extends Process {

        private final boolean alive;
        private final boolean waitInterrupted;
        private final boolean waitResult;
        private boolean destroyCalled;
        private boolean destroyForciblyCalled;

        private ControllableProcess(boolean alive, boolean waitInterrupted, boolean waitResult) {
            this.alive = alive;
            this.waitInterrupted = waitInterrupted;
            this.waitResult = waitResult;
        }

        @Override
        public OutputStream getOutputStream() {
            throw new UnsupportedOperationException();
        }

        @Override
        public InputStream getInputStream() {
            throw new UnsupportedOperationException();
        }

        @Override
        public InputStream getErrorStream() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            assertEquals(10, timeout);
            assertEquals(TimeUnit.SECONDS, unit);
            if (waitInterrupted) {
                throw new InterruptedException();
            }
            return waitResult;
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            destroyCalled = true;
        }

        @Override
        public Process destroyForcibly() {
            destroyForciblyCalled = true;
            return this;
        }

        @Override
        public boolean isAlive() {
            return alive;
        }
    }
}
