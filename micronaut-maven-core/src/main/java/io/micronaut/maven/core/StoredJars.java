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
package io.micronaut.maven.core;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

/**
 * Writes copies of JARs with every entry STORED, so that the JVM does not inflate the classes it reads from them.
 * The order, names, times, extra fields and comments of the entries are kept, so the same JAR always gives the same
 * bytes. Signed JARs, and JARs whose entries are all STORED already, are copied unchanged.
 *
 * <p>Internal to the Micronaut Maven plugin.</p>
 *
 * @since 5.1.0
 */
public final class StoredJars {

    private StoredJars() {
    }

    /**
     * Writes the STORED copy of a JAR, unless the target already is one: it has the source's modification time, which
     * every copy gets, the same entry names and no compressed entry.
     *
     * @param source the JAR
     * @param target the copy to write
     * @throws IOException if the JAR cannot be read or the copy cannot be written
     */
    public static void store(Path source, Path target) throws IOException {
        if (isCopyOf(target, source)) {
            return;
        }
        Files.createDirectories(target.toAbsolutePath().getParent());
        Path temporary = target.resolveSibling(target.getFileName() + ".tmp");
        try {
            try (var zip = new ZipFile(source.toFile())) {
                if (needsRewrite(zip)) {
                    write(zip, temporary);
                } else {
                    Files.copy(source, temporary, StandardCopyOption.REPLACE_EXISTING);
                }
            }
            Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING);
            Files.setLastModifiedTime(target, Files.getLastModifiedTime(source));
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private static boolean isCopyOf(Path target, Path source) throws IOException {
        if (!Files.isRegularFile(target) || !Files.getLastModifiedTime(target).equals(Files.getLastModifiedTime(source))) {
            return false;
        }
        try (var copy = new ZipFile(target.toFile()); var original = new ZipFile(source.toFile())) {
            if (needsRewrite(original) && needsRewrite(copy)) {
                return false;
            }
            return names(copy).equals(names(original));
        } catch (IOException e) {
            return false;
        }
    }

    private static Set<String> names(ZipFile zip) {
        Set<String> names = new HashSet<>();
        zip.stream().forEach(entry -> names.add(entry.getName()));
        return names;
    }

    private static boolean needsRewrite(ZipFile zip) {
        boolean deflated = false;
        Enumeration<? extends ZipEntry> entries = zip.entries();
        while (entries.hasMoreElements()) {
            ZipEntry entry = entries.nextElement();
            if (isSignature(entry.getName())) {
                return false;
            }
            deflated |= entry.getMethod() != ZipEntry.STORED;
        }
        return deflated;
    }

    private static boolean isSignature(String name) {
        String upper = name.toUpperCase(Locale.ROOT);
        if (!upper.startsWith("META-INF/") || upper.indexOf('/', "META-INF/".length()) >= 0) {
            return false;
        }
        return upper.endsWith(".SF") || upper.endsWith(".RSA") || upper.endsWith(".DSA") || upper.endsWith(".EC");
    }

    private static void write(ZipFile zip, Path target) throws IOException {
        try (var out = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(target)))) {
            if (zip.getComment() != null) {
                out.setComment(zip.getComment());
            }
            Set<String> written = new HashSet<>();
            Enumeration<? extends ZipEntry> entries = zip.entries();
            while (entries.hasMoreElements()) {
                String name = entries.nextElement().getName();
                if (written.add(name)) {
                    // For a name that occurs twice, the entry that a lookup by name returns
                    writeEntry(zip, zip.getEntry(name), out);
                }
            }
        }
    }

    private static void writeEntry(ZipFile zip, ZipEntry source, ZipOutputStream out) throws IOException {
        byte[] content;
        try (InputStream in = zip.getInputStream(source)) {
            content = in.readAllBytes();
        }
        var crc = new CRC32();
        crc.update(content);
        var entry = new ZipEntry(source.getName());
        entry.setMethod(ZipEntry.STORED);
        entry.setSize(content.length);
        entry.setCompressedSize(content.length);
        entry.setCrc(crc.getValue());
        entry.setTime(source.getTime());
        if (source.getExtra() != null) {
            entry.setExtra(source.getExtra());
        }
        if (source.getComment() != null) {
            entry.setComment(source.getComment());
        }
        out.putNextEntry(entry);
        out.write(content);
        out.closeEntry();
    }
}
