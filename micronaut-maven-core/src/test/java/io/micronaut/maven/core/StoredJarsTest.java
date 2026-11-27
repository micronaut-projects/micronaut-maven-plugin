package io.micronaut.maven.core;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class StoredJarsTest {

    private static final long TIME = 1_700_000_000_000L;

    @TempDir
    Path dir;

    @Test
    void storesEveryEntryAndKeepsOrderTimesAndContent() throws IOException {
        Path source = jar("lib.jar", "META-INF/MANIFEST.MF", "b/B.class", "a/A.class");
        Path target = dir.resolve("out/lib.jar");

        StoredJars.store(source, target);

        try (var zip = new ZipFile(target.toFile()); var original = new ZipFile(source.toFile())) {
            assertEquals(List.of("META-INF/MANIFEST.MF", "b/B.class", "a/A.class"), names(zip));
            zip.stream().forEach(entry -> {
                assertEquals(ZipEntry.STORED, entry.getMethod());
                assertEquals(original.getEntry(entry.getName()).getTime(), entry.getTime());
                assertArrayEquals(read(original, original.getEntry(entry.getName())), read(zip, entry));
            });
        }
        assertEquals(Files.getLastModifiedTime(source), Files.getLastModifiedTime(target));
    }

    @Test
    void sameJarGivesSameBytes() throws IOException {
        Path source = jar("lib.jar", "a/A.class", "b/B.class");
        Path first = dir.resolve("first/lib.jar");
        Path second = dir.resolve("second/lib.jar");

        StoredJars.store(source, first);
        StoredJars.store(source, second);

        assertArrayEquals(Files.readAllBytes(first), Files.readAllBytes(second));
    }

    @Test
    void copiesSignedJarsUnchanged() throws IOException {
        Path source = jar("signed.jar", "META-INF/MANIFEST.MF", "META-INF/SIGNER.SF", "META-INF/SIGNER.RSA", "a/A.class");
        Path target = dir.resolve("out/signed.jar");

        StoredJars.store(source, target);

        assertArrayEquals(Files.readAllBytes(source), Files.readAllBytes(target));
    }

    @Test
    void skipsAnUpToDateCopy() throws IOException {
        Path source = jar("lib.jar", "a/A.class");
        Path target = dir.resolve("out/lib.jar");
        StoredJars.store(source, target);
        FileTime written = Files.getLastModifiedTime(target);
        byte[] bytes = Files.readAllBytes(target);

        StoredJars.store(source, target);

        assertEquals(written, Files.getLastModifiedTime(target));
        assertArrayEquals(bytes, Files.readAllBytes(target));
    }

    @Test
    void rewritesACompressedTarget() throws IOException {
        Path source = jar("lib.jar", "a/A.class");
        Path target = dir.resolve("lib-copy.jar");
        Files.copy(source, target);
        Files.setLastModifiedTime(target, Files.getLastModifiedTime(source));

        StoredJars.store(source, target);

        try (var zip = new ZipFile(target.toFile())) {
            assertTrue(zip.stream().allMatch(entry -> entry.getMethod() == ZipEntry.STORED));
        }
        assertFalse(Files.exists(dir.resolve("lib-copy.jar.tmp")));
    }

    private Path jar(String name, String... entries) throws IOException {
        Path jar = dir.resolve(name);
        try (var out = new ZipOutputStream(Files.newOutputStream(jar))) {
            for (String entryName : entries) {
                var entry = new ZipEntry(entryName);
                entry.setTime(TIME);
                out.putNextEntry(entry);
                out.write(("content of " + entryName + " ".repeat(200)).getBytes(StandardCharsets.UTF_8));
                out.closeEntry();
            }
        }
        Files.setLastModifiedTime(jar, FileTime.fromMillis(TIME));
        return jar;
    }

    private static List<String> names(ZipFile zip) {
        var names = new ArrayList<String>();
        zip.stream().forEach(entry -> names.add(entry.getName()));
        return names;
    }

    private static byte[] read(ZipFile zip, ZipEntry entry) {
        try (var in = zip.getInputStream(entry)) {
            return in.readAllBytes();
        } catch (IOException e) {
            throw new AssertionError(e);
        }
    }
}
