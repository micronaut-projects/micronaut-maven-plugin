package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class StoredDependenciesPlanTest {

    @TempDir
    Path dir;

    @Test
    void replacesOnlyTheJarsOfTheDependencyLayers() throws IOException {
        Path jar = jar("lib-1.0.jar");
        Path snapshot = jar("snap-1.0-SNAPSHOT.jar");
        Path resource = Files.writeString(dir.resolve("application.yml"), "micronaut: {}");
        Instant time = Instant.ofEpochSecond(1);
        var dependencies = layer("dependencies", entry(jar, "/app/libs/lib-1.0.jar", time));
        var snapshots = layer("snapshot dependencies", entry(snapshot, "/app/libs/snap-1.0-SNAPSHOT.jar", time));
        var resources = layer("resources", entry(resource, "/app/resources/application.yml", time));

        List<LayerObject> layers = StoredDependenciesPlan.storeLayers(List.of(dependencies, snapshots, resources),
            dir.resolve("stored"));

        assertEquals(3, layers.size());
        assertSame(resources, layers.get(2));
        FileEntry stored = ((FileEntriesLayer) layers.get(0)).getEntries().get(0);
        assertEquals(dir.resolve("stored/dependencies/lib-1.0.jar"), stored.getSourceFile());
        assertEquals(AbsoluteUnixPath.get("/app/libs/lib-1.0.jar"), stored.getExtractionPath());
        assertEquals(time, stored.getModificationTime());
        assertEquals("dependencies", layers.get(0).getName());
        FileEntry storedSnapshot = ((FileEntriesLayer) layers.get(1)).getEntries().get(0);
        assertEquals(dir.resolve("stored/snapshot-dependencies/snap-1.0-SNAPSHOT.jar"), storedSnapshot.getSourceFile());
        try (var zip = new ZipFile(stored.getSourceFile().toFile())) {
            assertTrue(zip.stream().allMatch(entry -> entry.getMethod() == ZipEntry.STORED));
        }
    }

    private Path jar(String name) throws IOException {
        Path jar = dir.resolve(name);
        try (var out = new ZipOutputStream(Files.newOutputStream(jar))) {
            out.putNextEntry(new ZipEntry("a/A.class"));
            out.write("class".repeat(100).getBytes());
            out.closeEntry();
        }
        return jar;
    }

    private static FileEntry entry(Path source, String path, Instant time) {
        return new FileEntry(source, AbsoluteUnixPath.get(path), FilePermissions.DEFAULT_FILE_PERMISSIONS, time);
    }

    private static FileEntriesLayer layer(String name, FileEntry entry) {
        return FileEntriesLayer.builder().setName(name).addEntry(entry).build();
    }
}
