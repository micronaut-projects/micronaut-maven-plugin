package io.micronaut.maven.testresources;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}
