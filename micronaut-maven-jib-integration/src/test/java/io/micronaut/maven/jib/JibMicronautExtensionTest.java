package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import io.micronaut.maven.jib.JibMicronautExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.nio.file.Paths;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JibMicronautExtensionTest {

    @ParameterizedTest
    @CsvSource({
            "17.0.1, jre17-latest",
            "17.0.4.1, jre17-latest",
            "19.0.1, jre17-latest"
    })
    void testDetermineJavaVersion(String javaVersion, String expectedFnVersion) {
        String fnVersion = JibMicronautExtension.determineProjectFnVersion(javaVersion);
        assertEquals(expectedFnVersion, fnVersion);
    }

    @Test
    void testBuildProjectFnEntrypoint() {
        String entrypoint = String.join(" ", JibMicronautExtension.buildProjectFnEntrypoint());
        String expectedEntrypoint = "/usr/java/latest/bin/java -XX:-UsePerfData -XX:+UseSerialGC -Xshare:on -Djava.awt.headless=true -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint";
        assertEquals(expectedEntrypoint, entrypoint);
    }

    @Test
    void testRemapEntryForDependencies() {
        FileEntry entry = new FileEntry(
                Paths.get("app.jar"),
                AbsoluteUnixPath.get("/foo/bar/app.jar"),
                FilePermissions.DEFAULT_FILE_PERMISSIONS,
                Instant.now()
        );
        FileEntry remappedEntry = JibMicronautExtension.remapEntry(entry, "dependencies");

        assertEquals("/function/app/libs/app.jar", remappedEntry.getExtractionPath().toString());
    }

    @Test
    void testRemapEntryForResources() {
        FileEntry entry = new FileEntry(
                Paths.get("app.yml"),
                AbsoluteUnixPath.get("/foo/bar/app.yml"),
                FilePermissions.DEFAULT_FILE_PERMISSIONS,
                Instant.now()
        );
        FileEntry remappedEntry = JibMicronautExtension.remapEntry(entry, "resources");

        assertEquals("/function/foo/bar/app.yml", remappedEntry.getExtractionPath().toString());
    }

    @Test
    void testRemapLayer() {
        LayerObject layer = FileEntriesLayer.builder()
                .setName("dependencies")
                .addEntry(
                        Paths.get("app.jar"),
                        AbsoluteUnixPath.get("/foo/bar/app.jar"),
                        FilePermissions.DEFAULT_FILE_PERMISSIONS,
                        Instant.now()
                )
                .build();

        LayerObject remappedLayer = JibMicronautExtension.remapLayer(layer);

        assertEquals("dependencies", remappedLayer.getName());
    }
}
