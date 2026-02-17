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

import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Simple on-disk cache for configuration validation.
 */
final class ConfigurationValidationCache {
    private static final String KEY_INPUTS_FINGERPRINT = "inputsFingerprint";
    private static final String KEY_MAIN_RESOURCES_FINGERPRINT = "mainResourcesFingerprint";
    private static final String KEY_LAST_RESULT = "lastResult";

    private ConfigurationValidationCache() {
    }

    /**
     * @param cacheFile The cache file path
     * @param inputsFingerprint Fingerprint of all inputs (options + computed classpath)
     * @param resourcesFingerprint Fingerprint of resource directories relevant to this scenario
     * @return The cache entry if it matches the provided fingerprints, otherwise {@code null}
     */
    static CacheEntry readIfUpToDate(Path cacheFile, String inputsFingerprint, String resourcesFingerprint) {
        if (!Files.isRegularFile(cacheFile)) {
            return null;
        }
        Properties props = new Properties();
        try (InputStream is = Files.newInputStream(cacheFile)) {
            props.load(is);
        } catch (IOException e) {
            return null;
        }
        boolean matches = Objects.equals(inputsFingerprint, props.getProperty(KEY_INPUTS_FINGERPRINT))
            && Objects.equals(resourcesFingerprint, props.getProperty(KEY_MAIN_RESOURCES_FINGERPRINT));
        if (!matches) {
            return null;
        }

        String lastResult = props.getProperty(KEY_LAST_RESULT);
        LastResult parsed;
        try {
            parsed = lastResult != null ? LastResult.valueOf(lastResult) : LastResult.SUCCESS;
        } catch (IllegalArgumentException ignored) {
            parsed = LastResult.SUCCESS;
        }
        return new CacheEntry(parsed);
    }

    /**
     * Writes cache data to disk.
     *
     * @param cacheFile The cache file path
     * @param inputsFingerprint Fingerprint of all inputs (options + computed classpath)
     * @param resourcesFingerprint Fingerprint of resource directories relevant to this scenario
     * @throws IOException If writing fails
     */
    static void write(Path cacheFile, String inputsFingerprint, String resourcesFingerprint, LastResult lastResult) throws IOException {
        Files.createDirectories(cacheFile.getParent());
        Properties props = new Properties();
        props.setProperty(KEY_INPUTS_FINGERPRINT, inputsFingerprint);
        props.setProperty(KEY_MAIN_RESOURCES_FINGERPRINT, resourcesFingerprint);
        props.setProperty(KEY_LAST_RESULT, lastResult.name());
        try (OutputStream os = Files.newOutputStream(cacheFile)) {
            props.store(os, "Micronaut configuration validation cache");
        }
    }

    /**
     * Computes a fingerprint for multiple resource directories.
     *
     * @param resourceDirs The resource directories to fingerprint
     * @param ignorePatterns Resource patterns to ignore (glob syntax, relative to each resource directory)
     * @return A combined SHA-256 hex digest
     * @throws IOException If walking directories fails
     */
    static String fingerprintResources(List<Path> resourceDirs, @Nullable Iterable<String> ignorePatterns) throws IOException {
        if (resourceDirs == null || resourceDirs.isEmpty()) {
            return "no-resources";
        }
        MessageDigest digest = sha256();
        for (Path dir : resourceDirs) {
            update(digest, dir.toString());
            update(digest, fingerprintMainResources(dir, ignorePatterns));
        }
        return HexFormat.of().formatHex(digest.digest());
    }

    /**
     * Computes a best-effort fingerprint for the directory contents.
     *
     * @param mainResourcesDir The main resources directory
     * @return A SHA-256 hex digest, or "missing" if the directory is absent
     * @throws IOException If walking the directory fails
     */
    static String fingerprintMainResources(Path mainResourcesDir) throws IOException {
        return fingerprintMainResources(mainResourcesDir, null);
    }

    /**
     * Computes a best-effort fingerprint for the directory contents.
     *
     * @param mainResourcesDir The main resources directory
     * @param ignorePatterns Resource patterns to ignore (glob syntax, relative to main resources)
     * @return A SHA-256 hex digest, or "missing" if the directory is absent
     * @throws IOException If walking the directory fails
     */
    static String fingerprintMainResources(Path mainResourcesDir, @Nullable Iterable<String> ignorePatterns) throws IOException {
        if (mainResourcesDir == null || !Files.isDirectory(mainResourcesDir)) {
            return "missing";
        }
        List<Glob> globs = Glob.compile(ignorePatterns);
        MessageDigest digest = sha256();
        try (Stream<Path> s = Files.walk(mainResourcesDir)) {
            s.filter(Files::isRegularFile)
                .sorted()
                .forEach(p -> {
                    Path rel = mainResourcesDir.relativize(p);
                    if (Glob.matchesAny(globs, rel)) {
                        return;
                    }
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

    /**
     * Minimal glob matcher that operates on normalized {@code /}-separated relative paths.
     *
     * @param pattern The compiled regex pattern
     */
    private record Glob(Pattern pattern) {
        static List<Glob> compile(@Nullable Iterable<String> patterns) {
            if (patterns == null) {
                return List.of();
            }
            List<Glob> result = new java.util.ArrayList<>();
            for (String p : patterns) {
                if (p == null || p.isBlank()) {
                    continue;
                }
                result.add(new Glob(Pattern.compile(toRegex(p.trim()))));
            }
            return List.copyOf(result);
        }

        static boolean matchesAny(List<Glob> globs, Path relativePath) {
            if (globs == null || globs.isEmpty() || relativePath == null) {
                return false;
            }
            String normalized = relativePath.toString().replace('\\', '/');
            for (Glob g : globs) {
                if (g.pattern.matcher(normalized).matches()) {
                    return true;
                }
            }
            return false;
        }

        private static String toRegex(String glob) {
            // Supports '*' (segment), '**' (multi-segment), and '?' (single char) for /-separated paths.
            StringBuilder out = new StringBuilder(glob.length() * 2);
            out.append('^');
            for (int i = 0; i < glob.length(); i++) {
                char c = glob.charAt(i);
                if (c == '*') {
                    boolean doubleStar = (i + 1 < glob.length()) && glob.charAt(i + 1) == '*';
                    if (doubleStar) {
                        out.append(".*");
                        i++;
                    } else {
                        out.append("[^/]*");
                    }
                } else if (c == '?') {
                    out.append("[^/]");
                } else {
                    if (".()[]{}+$^|\\".indexOf(c) >= 0) {
                        out.append('\\');
                    }
                    out.append(c);
                }
            }
            out.append('$');
            return out.toString();
        }
    }

    /**
     * Cached result of the last validation execution.
     */
    enum LastResult {
        SUCCESS,
        FAILURE
    }

    record CacheEntry(LastResult lastResult) {
    }
}
