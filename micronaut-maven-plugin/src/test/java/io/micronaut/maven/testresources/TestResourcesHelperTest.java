package io.micronaut.maven.testresources;

import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.testresources.buildtools.ServerFactory;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ResourceLock("java.io.tmpdir")
class TestResourcesHelperTest {
    private static final String JAVA_IO_TMPDIR_PROPERTY = "java.io.tmpdir";

    @TempDir
    Path tempDir;

    @Test
    void updateApplicationTestPropertiesAddsScopeAndPreservesExistingEntries() throws IOException {
        Path applicationTestProperties = tempDir.resolve("application-test.properties");
        Files.writeString(applicationTestProperties, "existing=true\n");

        TestResourcesHelper.updateApplicationTestProperties(applicationTestProperties, "mvn-123.app1");

        Properties properties = new Properties();
        try (var input = Files.newInputStream(applicationTestProperties)) {
            properties.load(input);
        }
        assertEquals("true", properties.getProperty("existing"));
        assertEquals("mvn-123.app1", properties.getProperty("micronaut.test.resources.scope"));
    }

    @Test
    void sanitizeScopeSegmentNormalizesRelativePaths() {
        assertEquals("modules.app-1", TestResourcesHelper.sanitizeScopeSegment("modules/app 1"));
        assertEquals("modules.app-1", TestResourcesHelper.sanitizeScopeSegment("modules\\app 1"));
        assertEquals("app.1", TestResourcesHelper.sanitizeScopeSegment("---app.1---"));
        assertEquals("root", TestResourcesHelper.sanitizeScopeSegment("///"));
        assertEquals("root", TestResourcesHelper.sanitizeScopeSegment("\\\\\\"));
    }

    @Test
    void testOutputDirectoryUsesMavenBuildConfigurationWhenPresent() {
        MavenProject project = new MavenProject();
        Build build = new Build();
        build.setTestOutputDirectory(tempDir.resolve("custom-test-output").toString());
        project.setBuild(build);

        assertEquals(
            tempDir.resolve("custom-test-output"),
            TestResourcesHelper.testOutputDirectory(project, tempDir.toFile())
        );
    }

    @Test
    void testOutputDirectoryFallsBackToBuildDirectoryTestClasses() {
        MavenProject project = new MavenProject();

        assertEquals(
            tempDir.resolve("test-classes"),
            TestResourcesHelper.testOutputDirectory(project, tempDir.toFile())
        );
    }

    @Test
    void sharedServerLockBlocksSamePathAndCleansUpAfterRelease() throws Exception {
        Path serverSettingsDirectory = tempDir.resolve("shared-settings");
        CountDownLatch started = new CountDownLatch(1);
        CountDownLatch acquired = new CountDownLatch(1);
        Thread worker;

        try (var ignored = TestResourcesHelper.sharedServerLock(serverSettingsDirectory)) {
            worker = new Thread(() -> {
                started.countDown();
                try (var ignoredWorker = TestResourcesHelper.sharedServerLock(serverSettingsDirectory)) {
                    acquired.countDown();
                }
            });
            worker.start();

            assertTrue(started.await(5, TimeUnit.SECONDS));
            assertFalse(acquired.await(200, TimeUnit.MILLISECONDS));
        }

        assertTrue(acquired.await(5, TimeUnit.SECONDS));
        worker.join();
        assertEquals(0, TestResourcesHelper.sharedServerLockCount());
    }

    @Test
    void keepAliveFileUsesSessionScopedRandomizedDirectory() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        withTmpDir(scopedTmpDir, () -> {
            TestResourcesHelper firstHelper = helper("builder-123");
            TestResourcesHelper secondHelper = helper("builder-123");

            Path firstKeepAlive = invokeGetKeepAliveFile(firstHelper);
            Path secondKeepAlive = invokeGetKeepAliveFile(secondHelper);
            Path firstKeepAliveAgain = invokeGetKeepAliveFile(firstHelper);

            assertEquals(firstKeepAlive, firstKeepAliveAgain);
            assertTrue(firstKeepAlive.startsWith(scopedTmpDir));
            assertEquals("keepalive-builder-123", firstKeepAlive.getFileName().toString());
            assertTrue(firstKeepAlive.getParent().getFileName().toString().startsWith("mn-test-resources-"));
            assertFalse(firstKeepAlive.getParent().equals(scopedTmpDir));
            assertFalse(firstKeepAlive.getParent().equals(secondKeepAlive.getParent()));
        });
    }

    @Test
    void createKeepAliveFileDoesNotWriteThroughExistingSymlink() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));
        Path protectedFile = tempDir.resolve("protected.txt");

        withTmpDir(scopedTmpDir, () -> {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);
            Files.createDirectories(keepAliveFile.getParent());

            assumeTrue(createSymlinkIfSupported(keepAliveFile, protectedFile), "Symbolic links are not available");

            InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> invokeCreateKeepAliveFile(helper));

            assertInstanceOf(IOException.class, exception.getCause());
            assertFalse(Files.exists(protectedFile));
            assertFalse(invokeIsKeepAlive(helper));
            assertTrue(Files.isSymbolicLink(keepAliveFile));
        });
    }

    @Test
    void deleteKeepAliveFileRemovesEmptyKeepAliveDirectory() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        withTmpDir(scopedTmpDir, () -> {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);

            invokeCreateKeepAliveFile(helper);
            invokeDeleteKeepAliveFile(helper);

            assertFalse(Files.exists(keepAliveFile, LinkOption.NOFOLLOW_LINKS));
            assertFalse(Files.exists(keepAliveFile.getParent(), LinkOption.NOFOLLOW_LINKS));
        });
    }

    @Test
    void createKeepAliveDirectoryRejectsExistingNonDirectoryPath() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        withTmpDir(scopedTmpDir, () -> {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveDirectory = invokeGetKeepAliveFile(helper).getParent();
            Files.writeString(keepAliveDirectory, "not-a-directory");

            InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> invokeCreateKeepAliveDirectory(helper));

            assertInstanceOf(IOException.class, exception.getCause());
        });
    }

    @Test
    void deleteKeepAliveFileLeavesDirectoryWhenOtherFilesRemain() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        withTmpDir(scopedTmpDir, () -> {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);
            Path siblingFile = keepAliveFile.getParent().resolve("sibling.txt");

            invokeCreateKeepAliveFile(helper);
            Files.writeString(siblingFile, "keep");

            invokeDeleteKeepAliveFile(helper);

            assertFalse(Files.exists(keepAliveFile, LinkOption.NOFOLLOW_LINKS));
            assertTrue(Files.exists(keepAliveFile.getParent(), LinkOption.NOFOLLOW_LINKS));
            assertTrue(Files.exists(siblingFile, LinkOption.NOFOLLOW_LINKS));
        });
    }

    @Test
    void keepAliveDirectoryAttributesOnlyUsePosixPermissionsWhenSupported() throws Exception {
        FileAttribute<?>[] attributes = invokeKeepAliveDirectoryAttributes(tempDir);
        boolean posixSupported = Files.getFileStore(tempDir).supportsFileAttributeView("posix");

        assertEquals(posixSupported ? 1 : 0, attributes.length);
    }

    @Test
    void keepAliveDirectoryAttributesReturnsEmptyArrayForNullDirectory() throws Exception {
        assertEquals(0, invokeKeepAliveDirectoryAttributes(null).length);
    }

    @Test
    void doStartReusesSessionSharedServerSettings() throws Exception {
        Path serverSettingsDirectory = tempDir.resolve("shared-settings");
        Path buildDirectory = tempDir.resolve("build");
        Path testOutputDirectory = tempDir.resolve("test-output");
        int port = 12345;
        ServerSettings serverSettings = new ServerSettings(port, "test-token", 30, null);
        ServerUtils.writeServerSettings(serverSettingsDirectory, serverSettings);

        MavenProject project = new MavenProject();
        project.setArtifactId("app1");
        project.setFile(tempDir.resolve("app1").resolve("pom.xml").toFile());
        Build build = new Build();
        build.setTestOutputDirectory(testOutputDirectory.toString());
        project.setBuild(build);

        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        when(request.getMultiModuleProjectDirectory()).thenReturn(tempDir.toFile());
        MavenSession mavenSession = mock(MavenSession.class);
        when(mavenSession.getRequest()).thenReturn(request);

        TestResourcesHelper helper = new TestResourcesHelper(
            true,
            true,
            buildDirectory.toFile(),
            null,
            null,
            null,
            project,
            mavenSession,
            null,
            null,
            "4.0.0",
            false,
            null,
            null,
            false,
            false,
            Map.of()
        );

        assertTrue(invokeRegisterSharedServerUse(helper, serverSettingsDirectory, port, true));

        ServerFactory serverFactory = new ServerFactory() {
            @Override
            public void startServer(ServerUtils.ProcessParameters processParameters) {
                throw new AssertionError("Existing session shared server should be reused");
            }

            @Override
            public void waitFor(java.time.Duration timeout) {
                throw new AssertionError("Existing session shared server should be reused");
            }
        };

        String previousServerUri = System.getProperty("micronaut.test.resources.server.uri");
        String previousAccessToken = System.getProperty("micronaut.test.resources.server.access.token");
        String previousReadTimeout = System.getProperty("micronaut.test.resources.server.client.read.timeout");
        try {
            invokeDoStart(helper, buildDirectory, serverSettingsDirectory, serverFactory);

            assertEquals(Integer.toString(port), Files.readString(buildDirectory.resolve("test-resources-port.txt")));
            assertEquals("http://localhost:" + port, System.getProperty("micronaut.test.resources.server.uri"));
            assertEquals("test-token", System.getProperty("micronaut.test.resources.server.access.token"));
            assertEquals("30", System.getProperty("micronaut.test.resources.server.client.read.timeout"));

            Properties properties = new Properties();
            try (var input = Files.newInputStream(testOutputDirectory.resolve("application-test.properties"))) {
                properties.load(input);
            }
            assertTrue(properties.getProperty("micronaut.test.resources.scope").startsWith("mvn-"));
            assertTrue(properties.getProperty("micronaut.test.resources.scope").endsWith(".app1"));
        } finally {
            restoreSystemProperty("micronaut.test.resources.server.uri", previousServerUri);
            restoreSystemProperty("micronaut.test.resources.server.access.token", previousAccessToken);
            restoreSystemProperty("micronaut.test.resources.server.client.read.timeout", previousReadTimeout);
        }
    }

    @Test
    void doStartReusesReachableRecordedStandaloneServerBeforeFallback() throws Exception {
        Path serverSettingsDirectory = tempDir.resolve("standalone-settings");
        Path buildDirectory = tempDir.resolve("build");

        try (ServerSocket server = new ServerSocket(0)) {
            int port = server.getLocalPort();
            ServerUtils.writeServerSettings(serverSettingsDirectory, new ServerSettings(port, "standalone-token", 45, 60));
            TestResourcesHelper helper = helperWithDependencyResolution(buildDirectory, null);
            AtomicBoolean serverStarted = new AtomicBoolean(false);
            ServerFactory serverFactory = new ServerFactory() {
                @Override
                public void startServer(ServerUtils.ProcessParameters processParameters) {
                    throw new AssertionError("Reachable recorded standalone server should be reused before fallback");
                }

                @Override
                public void waitFor(Duration timeout) {
                    throw new AssertionError("Reachable recorded standalone server should be reused before fallback");
                }
            };

            String previousServerUri = System.getProperty("micronaut.test.resources.server.uri");
            String previousAccessToken = System.getProperty("micronaut.test.resources.server.access.token");
            String previousReadTimeout = System.getProperty("micronaut.test.resources.server.client.read.timeout");
            try {
                invokeDoStart(helper, buildDirectory, serverSettingsDirectory, serverFactory, serverStarted);

                assertFalse(serverStarted.get());
                assertEquals(Integer.toString(port), Files.readString(buildDirectory.resolve("test-resources-port.txt")));
                assertEquals("http://localhost:" + port, System.getProperty("micronaut.test.resources.server.uri"));
                assertEquals("standalone-token", System.getProperty("micronaut.test.resources.server.access.token"));
                assertEquals("45", System.getProperty("micronaut.test.resources.server.client.read.timeout"));
                assertTrue(invokeIsKeepAlive(helper), "Ordinary builds must leave reused standalone servers alive during cleanup");
            } finally {
                restoreSystemProperty("micronaut.test.resources.server.uri", previousServerUri);
                restoreSystemProperty("micronaut.test.resources.server.access.token", previousAccessToken);
                restoreSystemProperty("micronaut.test.resources.server.client.read.timeout", previousReadTimeout);
            }
        }
    }

    @Test
    void doStartFallsBackWhenRecordedStandaloneServerIsStale() throws Exception {
        Path serverSettingsDirectory = tempDir.resolve("standalone-settings");
        Path buildDirectory = tempDir.resolve("build");
        int stalePort = availableTcpPort();
        int fallbackPort = availableTcpPort();
        ServerUtils.writeServerSettings(serverSettingsDirectory, new ServerSettings(stalePort, "stale-token", 45, 60));
        TestResourcesHelper helper = helperWithDependencyResolution(buildDirectory, fallbackPort);
        AtomicBoolean fallbackInvoked = new AtomicBoolean(false);
        AtomicBoolean serverStarted = new AtomicBoolean(false);
        ServerFactory serverFactory = new ServerFactory() {
            @Override
            public void startServer(ServerUtils.ProcessParameters processParameters) {
                fallbackInvoked.set(true);
                serverStarted.set(true);
            }

            @Override
            public void waitFor(Duration timeout) {
                // No-op; explicit fallback ports do not need a port-file wait.
            }
        };

        invokeDoStart(helper, buildDirectory, serverSettingsDirectory, serverFactory, serverStarted);

        assertTrue(fallbackInvoked.get());
        assertEquals(Integer.toString(fallbackPort), Files.readString(buildDirectory.resolve("test-resources-port.txt")));
    }

    @Test
    void findSessionSharedServerRequiresSharedMode() throws Exception {
        TestResourcesHelper helper = new TestResourcesHelper(mock(MavenSession.class), true, false, tempDir.toFile());

        assertTrue(invokeFindSessionSharedServer(helper, tempDir.resolve("shared-settings")).isEmpty());
    }

    @Test
    void findSessionSharedServerRequiresRegisteredSessionState() throws Exception {
        TestResourcesHelper helper = sharedHelper(tempDir.resolve("build"), new MavenProject(), mock(MavenSession.class));

        assertTrue(invokeFindSessionSharedServer(helper, tempDir.resolve("shared-settings")).isEmpty());
    }

    @Test
    void findSessionSharedServerRejectsMismatchedSettingsPort() throws Exception {
        Path serverSettingsDirectory = tempDir.resolve("shared-settings");
        int registeredPort = 12345;
        ServerUtils.writeServerSettings(serverSettingsDirectory, new ServerSettings(54321, "test-token", 30));

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("app1").resolve("pom.xml").toFile());
        TestResourcesHelper helper = sharedHelper(tempDir.resolve("build"), project, mock(MavenSession.class));

        assertTrue(invokeRegisterSharedServerUse(helper, serverSettingsDirectory, registeredPort, true));
        assertTrue(invokeFindSessionSharedServer(helper, serverSettingsDirectory).isEmpty());
    }

    private static TestResourcesHelper sharedHelper(Path buildDirectory, MavenProject project, MavenSession mavenSession) {
        return new TestResourcesHelper(
            true,
            true,
            buildDirectory.toFile(),
            null,
            null,
            null,
            project,
            mavenSession,
            null,
            null,
            "4.0.0",
            false,
            null,
            null,
            false,
            false,
            Map.of()
        );
    }

    private static TestResourcesHelper helper(String builderId) {
        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        when(request.getBuilderId()).thenReturn(builderId);

        MavenSession mavenSession = mock(MavenSession.class);
        when(mavenSession.getRequest()).thenReturn(request);

        return new TestResourcesHelper(mavenSession, true, false, new File("."));
    }

    private static TestResourcesHelper helperWithDependencyResolution(Path buildDirectory, Integer explicitPort) throws Exception {
        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        when(request.getBuilderId()).thenReturn("builder-123");
        MavenSession mavenSession = mock(MavenSession.class);
        when(mavenSession.getRequest()).thenReturn(request);
        when(mavenSession.getGoals()).thenReturn(List.of("test"));
        DependencyResolutionService dependencyResolutionService = mock(DependencyResolutionService.class);
        when(dependencyResolutionService.artifactResultsFor(any(), eq(true))).thenReturn(List.of());
        return new TestResourcesHelper(
            true,
            false,
            buildDirectory.toFile(),
            explicitPort,
            null,
            null,
            new MavenProject(),
            mavenSession,
            dependencyResolutionService,
            null,
            "4.0.0",
            false,
            null,
            null,
            false,
            false,
            Map.of()
        );
    }

    private static int availableTcpPort() throws IOException {
        try (ServerSocket server = new ServerSocket(0)) {
            return server.getLocalPort();
        }
    }

    private static Path invokeGetKeepAliveFile(TestResourcesHelper helper) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("getKeepAliveFile");
        method.setAccessible(true);
        return (Path) method.invoke(helper);
    }

    private static void invokeCreateKeepAliveFile(TestResourcesHelper helper) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("createKeepAliveFile");
        method.setAccessible(true);
        method.invoke(helper);
    }

    private static boolean invokeIsKeepAlive(TestResourcesHelper helper) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("isKeepAlive");
        method.setAccessible(true);
        return (boolean) method.invoke(helper);
    }

    private static void invokeDeleteKeepAliveFile(TestResourcesHelper helper) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("deleteKeepAliveFile");
        method.setAccessible(true);
        method.invoke(helper);
    }

    private static void invokeCreateKeepAliveDirectory(TestResourcesHelper helper) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("createKeepAliveDirectory");
        method.setAccessible(true);
        method.invoke(helper);
    }

    private static boolean createSymlinkIfSupported(Path link, Path target) {
        try {
            Files.createSymbolicLink(link, target);
            return true;
        } catch (UnsupportedOperationException | IOException e) {
            return false;
        }
    }

    private static void withTmpDir(Path scopedTmpDir, ThrowingRunnable action) throws Exception {
        String previousTmpDir = System.getProperty(JAVA_IO_TMPDIR_PROPERTY);
        System.setProperty(JAVA_IO_TMPDIR_PROPERTY, scopedTmpDir.toString());
        try {
            action.run();
        } finally {
            restoreSystemProperty(JAVA_IO_TMPDIR_PROPERTY, previousTmpDir);
        }
    }

    private static void restoreSystemProperty(String propertyName, String previousValue) {
        if (previousValue == null) {
            System.clearProperty(propertyName);
        } else {
            System.setProperty(propertyName, previousValue);
        }
    }

    private static FileAttribute<?>[] invokeKeepAliveDirectoryAttributes(Path directory) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("keepAliveDirectoryAttributes", Path.class);
        method.setAccessible(true);
        return (FileAttribute<?>[]) method.invoke(null, directory);
    }

    private static boolean invokeRegisterSharedServerUse(TestResourcesHelper helper, Path serverSettingsDirectory, int port, boolean serverStarted) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("registerSharedServerUse", Path.class, int.class, boolean.class);
        method.setAccessible(true);
        return (boolean) method.invoke(helper, serverSettingsDirectory, port, serverStarted);
    }

    @SuppressWarnings("unchecked")
    private static Optional<ServerSettings> invokeFindSessionSharedServer(TestResourcesHelper helper, Path serverSettingsDirectory) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("findSessionSharedServer", Path.class);
        method.setAccessible(true);
        return (Optional<ServerSettings>) method.invoke(helper, serverSettingsDirectory);
    }

    private static void invokeDoStart(TestResourcesHelper helper, Path buildDirectory, Path serverSettingsDirectory, ServerFactory serverFactory) throws Exception {
        invokeDoStart(helper, buildDirectory, serverSettingsDirectory, serverFactory, new AtomicBoolean(false));
    }

    private static void invokeDoStart(TestResourcesHelper helper, Path buildDirectory, Path serverSettingsDirectory, ServerFactory serverFactory, AtomicBoolean serverStarted) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod(
            "doStart",
            String.class,
            Path.class,
            Path.class,
            ServerFactory.class,
            AtomicBoolean.class
        );
        method.setAccessible(true);
        method.invoke(helper, "new-token", buildDirectory, serverSettingsDirectory, serverFactory, serverStarted);
    }

    @FunctionalInterface
    private interface ThrowingRunnable {
        void run() throws Exception;
    }
}
