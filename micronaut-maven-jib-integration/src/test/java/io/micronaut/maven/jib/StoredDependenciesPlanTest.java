package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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

    @Test
    void appliesOnlyWhenTheDockerGoalAsksForIt() throws Exception {
        Path jar = jar("lib-1.0.jar");
        var plan = ContainerBuildPlan.builder()
            .addLayer(layer("dependencies", entry(jar, "/app/libs/lib-1.0.jar", Instant.ofEpochSecond(1))))
            .build();
        var logs = new ArrayList<String>();
        ExtensionLogger logger = (level, message) -> logs.add(message);

        var unchanged = plan.toBuilder();
        StoredDependenciesPlan.apply(plan, unchanged, mavenData(new Properties()), logger);
        assertEquals(jar, ((FileEntriesLayer) unchanged.build().getLayers().get(0)).getEntries().get(0).getSourceFile());
        assertEquals(List.of(), logs);

        var properties = new Properties();
        properties.setProperty(StoredDependenciesPlan.DIRECTORY_PROPERTY, dir.resolve("stored").toString());
        var stored = plan.toBuilder();
        StoredDependenciesPlan.apply(plan, stored, mavenData(properties), logger);
        assertEquals(dir.resolve("stored/dependencies/lib-1.0.jar"),
            ((FileEntriesLayer) stored.build().getLayers().get(0)).getEntries().get(0).getSourceFile());
        assertEquals(List.of("Writing the dependency JARs with STORED entries"), logs);
    }

    @Test
    void failsTheBuildWhenACopyCannotBeWritten() throws Exception {
        Path jar = jar("lib-1.0.jar");
        var plan = ContainerBuildPlan.builder()
            .addLayer(layer("dependencies", entry(jar, "/app/libs/lib-1.0.jar", Instant.ofEpochSecond(1))))
            .build();
        // A file where the directory of the copies should be
        Path blocked = Files.writeString(dir.resolve("blocked"), "");
        var properties = new Properties();
        properties.setProperty(StoredDependenciesPlan.DIRECTORY_PROPERTY, blocked.toString());

        var e = assertThrows(JibPluginExtensionException.class,
            () -> StoredDependenciesPlan.apply(plan, plan.toBuilder(), mavenData(properties), (level, message) -> { }));

        assertTrue(e.getMessage().startsWith("Could not write the STORED copies of the dependency JARs"), e.getMessage());
    }

    @Test
    void keepsTheFilesOfADependencyLayerThatAreNotJars() throws IOException {
        Path pom = Files.writeString(dir.resolve("lib-1.0.pom"), "<project/>");
        var dependencies = layer("dependencies", entry(pom, "/app/libs/lib-1.0.pom", Instant.ofEpochSecond(1)));

        List<LayerObject> layers = StoredDependenciesPlan.storeLayers(List.of(dependencies), dir.resolve("stored"));

        assertEquals(pom, ((FileEntriesLayer) layers.get(0)).getEntries().get(0).getSourceFile());
    }

    private static MavenData mavenData(Properties properties) {
        var project = mock(MavenProject.class);
        when(project.getProperties()).thenReturn(properties);
        return new MavenData() {
            @Override
            public MavenProject getMavenProject() {
                return project;
            }

            @Override
            public MavenSession getMavenSession() {
                return mock(MavenSession.class);
            }
        };
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
