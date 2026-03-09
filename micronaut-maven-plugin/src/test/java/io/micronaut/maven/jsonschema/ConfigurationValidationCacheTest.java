package io.micronaut.maven.jsonschema;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

class ConfigurationValidationCacheTest {

    @TempDir
    Path tempDir;

    @Test
    void readIfUpToDateReturnsNullForMissingFile() {
        Path cacheFile = tempDir.resolve("cache.properties");

        ConfigurationValidationCache.CacheEntry entry = ConfigurationValidationCache.readIfUpToDate(cacheFile, "in", "res");

        assertNull(entry);
    }

    @Test
    void writeAndReadRoundTripWorks() throws IOException {
        Path cacheFile = tempDir.resolve("cache.properties");

        ConfigurationValidationCache.write(cacheFile, "in", "res", ConfigurationValidationCache.LastResult.FAILURE);
        ConfigurationValidationCache.CacheEntry entry = ConfigurationValidationCache.readIfUpToDate(cacheFile, "in", "res");

        assertNotNull(entry);
        assertEquals(ConfigurationValidationCache.LastResult.FAILURE, entry.lastResult());
    }

    @Test
    void readIfUpToDateReturnsNullForMismatchedFingerprints() throws IOException {
        Path cacheFile = tempDir.resolve("cache.properties");
        ConfigurationValidationCache.write(cacheFile, "in", "res", ConfigurationValidationCache.LastResult.SUCCESS);

        assertNull(ConfigurationValidationCache.readIfUpToDate(cacheFile, "other", "res"));
        assertNull(ConfigurationValidationCache.readIfUpToDate(cacheFile, "in", "other"));
    }

    @Test
    void fingerprintDirectoryContentsReturnsMissingForAbsentDirectory() throws IOException {
        String fingerprint = ConfigurationValidationCache.fingerprintDirectoryContents(tempDir.resolve("does-not-exist"));

        assertEquals("missing", fingerprint);
    }

    @Test
    void fingerprintMainResourcesRespectsIgnorePatterns() throws IOException {
        Path resourcesDir = Files.createDirectories(tempDir.resolve("resources"));
        Path included = resourcesDir.resolve("application.yml");
        Path ignored = resourcesDir.resolve("META-INF/info.txt");
        Files.createDirectories(ignored.getParent());
        Files.writeString(included, "a: 1");
        Files.writeString(ignored, "ignored");

        String withoutIgnore = ConfigurationValidationCache.fingerprintMainResources(resourcesDir, null);
        String withIgnore = ConfigurationValidationCache.fingerprintMainResources(resourcesDir, List.of("META-INF/*"));

        assertNotEquals(withoutIgnore, withIgnore);
    }

    @Test
    void fingerprintClasspathHandlesBlankInvalidAndMissingElements() {
        String fingerprint = ConfigurationValidationCache.fingerprintClasspath(List.of("", "\u0000invalid", tempDir.resolve("missing.jar").toString()));

        assertNotNull(fingerprint);
        assertNotEquals("no-classpath", fingerprint);
    }
}
