package io.micronaut.maven.testresources;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
        assertEquals("root", TestResourcesHelper.sanitizeScopeSegment("///"));
    }
}
