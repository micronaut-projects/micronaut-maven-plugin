/*
 * Copyright 2017-2026 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven.jsonschema;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * Simple on-disk cache for configuration validation.
 */
final class ConfigurationValidationCache {
    private static final String KEY_INPUTS_FINGERPRINT = "inputsFingerprint";
    private static final String KEY_MAIN_RESOURCES_FINGERPRINT = "mainResourcesFingerprint";

    private ConfigurationValidationCache() {
    }

    /**
     * @param cacheFile The cache file path
     * @param inputsFingerprint Fingerprint of all inputs (options + computed classpath)
     * @param mainResourcesFingerprint Fingerprint of {@code src/main/resources}
     * @return {@code true} if the cache matches the provided fingerprints
     */
    static boolean isUpToDate(Path cacheFile, String inputsFingerprint, String mainResourcesFingerprint) {
        if (!Files.isRegularFile(cacheFile)) {
            return false;
        }
        Properties props = new Properties();
        try (InputStream is = Files.newInputStream(cacheFile)) {
            props.load(is);
        } catch (IOException e) {
            return false;
        }
        return Objects.equals(inputsFingerprint, props.getProperty(KEY_INPUTS_FINGERPRINT))
            && Objects.equals(mainResourcesFingerprint, props.getProperty(KEY_MAIN_RESOURCES_FINGERPRINT));
    }

    /**
     * Writes cache data to disk.
     *
     * @param cacheFile The cache file path
     * @param inputsFingerprint Fingerprint of all inputs (options + computed classpath)
     * @param mainResourcesFingerprint Fingerprint of {@code src/main/resources}
     * @throws IOException If writing fails
     */
    static void write(Path cacheFile, String inputsFingerprint, String mainResourcesFingerprint) throws IOException {
        Files.createDirectories(cacheFile.getParent());
        Properties props = new Properties();
        props.setProperty(KEY_INPUTS_FINGERPRINT, inputsFingerprint);
        props.setProperty(KEY_MAIN_RESOURCES_FINGERPRINT, mainResourcesFingerprint);
        try (OutputStream os = Files.newOutputStream(cacheFile)) {
            props.store(os, "Micronaut configuration validation cache");
        }
    }

    /**
     * Computes a best-effort fingerprint for the directory contents.
     *
     * @param mainResourcesDir The main resources directory
     * @return A SHA-256 hex digest, or "missing" if the directory is absent
     * @throws IOException If walking the directory fails
     */
    static String fingerprintMainResources(Path mainResourcesDir) throws IOException {
        if (mainResourcesDir == null || !Files.isDirectory(mainResourcesDir)) {
            return "missing";
        }
        MessageDigest digest = sha256();
        try (Stream<Path> s = Files.walk(mainResourcesDir)) {
            s.filter(Files::isRegularFile)
                .sorted()
                .forEach(p -> {
                    Path rel = mainResourcesDir.relativize(p);
                    update(digest, rel.toString());
                    try {
                        // Include contents to avoid false cache hits when timestamps don't change (coarse FS resolution).
                        update(digest, Long.toString(Files.size(p)));
                        try (InputStream is = Files.newInputStream(p)) {
                            byte[] buffer = new byte[8192];
                            int len;
                            while ((len = is.read(buffer)) > -1) {
                                digest.update(buffer, 0, len);
                            }
                        }
                    } catch (IOException ignored) {
                        // best effort
                        update(digest, "<unreadable>");
                    }
                });
        }
        return HexFormat.of().formatHex(digest.digest());
    }

    private static void update(MessageDigest digest, String s) {
        digest.update(s.getBytes(StandardCharsets.UTF_8));
        digest.update((byte) 0);
    }

    private static MessageDigest sha256() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
