package io.micronaut.maven.testresources;

import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class TestResourcesHelperTest {

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

        String previousTmpDir = System.getProperty("java.io.tmpdir");
        System.setProperty("java.io.tmpdir", scopedTmpDir.toString());
        try {
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
        } finally {
            if (previousTmpDir == null) {
                System.clearProperty("java.io.tmpdir");
            } else {
                System.setProperty("java.io.tmpdir", previousTmpDir);
            }
        }
    }

    @Test
    void createKeepAliveFileDoesNotWriteThroughExistingSymlink() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));
        Path protectedFile = tempDir.resolve("protected.txt");

        String previousTmpDir = System.getProperty("java.io.tmpdir");
        System.setProperty("java.io.tmpdir", scopedTmpDir.toString());
        try {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);
            Files.createDirectories(keepAliveFile.getParent());

            assumeTrue(createSymlinkIfSupported(keepAliveFile, protectedFile), "Symbolic links are not available");

            InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> invokeCreateKeepAliveFile(helper));

            assertInstanceOf(IOException.class, exception.getCause());
            assertFalse(Files.exists(protectedFile));
            assertFalse(invokeIsKeepAlive(helper));
            assertTrue(Files.isSymbolicLink(keepAliveFile));
        } finally {
            if (previousTmpDir == null) {
                System.clearProperty("java.io.tmpdir");
            } else {
                System.setProperty("java.io.tmpdir", previousTmpDir);
            }
        }
    }

    @Test
    void deleteKeepAliveFileRemovesEmptyKeepAliveDirectory() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        String previousTmpDir = System.getProperty("java.io.tmpdir");
        System.setProperty("java.io.tmpdir", scopedTmpDir.toString());
        try {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);

            invokeCreateKeepAliveFile(helper);
            invokeDeleteKeepAliveFile(helper);

            assertFalse(Files.exists(keepAliveFile, LinkOption.NOFOLLOW_LINKS));
            assertFalse(Files.exists(keepAliveFile.getParent(), LinkOption.NOFOLLOW_LINKS));
        } finally {
            if (previousTmpDir == null) {
                System.clearProperty("java.io.tmpdir");
            } else {
                System.setProperty("java.io.tmpdir", previousTmpDir);
            }
        }
    }

    @Test
    void createKeepAliveDirectoryRejectsExistingNonDirectoryPath() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        String previousTmpDir = System.getProperty("java.io.tmpdir");
        System.setProperty("java.io.tmpdir", scopedTmpDir.toString());
        try {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveDirectory = invokeGetKeepAliveFile(helper).getParent();
            Files.writeString(keepAliveDirectory, "not-a-directory");

            InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> invokeCreateKeepAliveDirectory(helper));

            assertInstanceOf(IOException.class, exception.getCause());
        } finally {
            if (previousTmpDir == null) {
                System.clearProperty("java.io.tmpdir");
            } else {
                System.setProperty("java.io.tmpdir", previousTmpDir);
            }
        }
    }

    @Test
    void deleteKeepAliveFileLeavesDirectoryWhenOtherFilesRemain() throws Exception {
        Path scopedTmpDir = Files.createDirectory(tempDir.resolve("tmp"));

        String previousTmpDir = System.getProperty("java.io.tmpdir");
        System.setProperty("java.io.tmpdir", scopedTmpDir.toString());
        try {
            TestResourcesHelper helper = helper("builder-123");
            Path keepAliveFile = invokeGetKeepAliveFile(helper);
            Path siblingFile = keepAliveFile.getParent().resolve("sibling.txt");

            invokeCreateKeepAliveFile(helper);
            Files.writeString(siblingFile, "keep");

            invokeDeleteKeepAliveFile(helper);

            assertFalse(Files.exists(keepAliveFile, LinkOption.NOFOLLOW_LINKS));
            assertTrue(Files.exists(keepAliveFile.getParent(), LinkOption.NOFOLLOW_LINKS));
            assertTrue(Files.exists(siblingFile, LinkOption.NOFOLLOW_LINKS));
        } finally {
            if (previousTmpDir == null) {
                System.clearProperty("java.io.tmpdir");
            } else {
                System.setProperty("java.io.tmpdir", previousTmpDir);
            }
        }
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

    private static TestResourcesHelper helper(String builderId) {
        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        when(request.getBuilderId()).thenReturn(builderId);

        MavenSession mavenSession = mock(MavenSession.class);
        when(mavenSession.getRequest()).thenReturn(request);

        return new TestResourcesHelper(mavenSession, true, false, new File("."));
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

    private static FileAttribute<?>[] invokeKeepAliveDirectoryAttributes(Path directory) throws Exception {
        Method method = TestResourcesHelper.class.getDeclaredMethod("keepAliveDirectoryAttributes", Path.class);
        method.setAccessible(true);
        return (FileAttribute<?>[]) method.invoke(null, directory);
    }
}
