package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import io.micronaut.maven.core.DockerBuildStrategy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.nio.file.Paths;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JibMicronautExtensionTest {

    @ParameterizedTest
    @CsvSource({
            "17.0.1,    17-jre",
            "17.0.4.1,  17-jre",
            "19.0.1,    21-jre",
            "21.0.1,    21-jre"
    })
    void testDetermineJavaVersion(String javaVersion, String expectedFnVersion) {
        String fnVersion = JibMicronautExtension.determineProjectFnVersion(javaVersion);
        assertEquals(expectedFnVersion, fnVersion);
    }

    @Test
    void testBuildProjectFnEntrypoint() {
        String entrypoint = String.join(" ", JibMicronautExtension.buildProjectFnEntrypoint());
        String expectedEntrypoint = "java -XX:-UsePerfData -XX:+UseSerialGC -Xshare:auto -Djava.awt.headless=true -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint";
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

    @ParameterizedTest
    @CsvSource({
            "DEFAULT,           17, eclipse-temurin:17-jre",
            "ORACLE_FUNCTION,   17, eclipse-temurin:17-jre",
            "LAMBDA,            17, public.ecr.aws/lambda/java:17",

            "DEFAULT,           21, eclipse-temurin:21-jre",
            "ORACLE_FUNCTION,   21, eclipse-temurin:21-jre",
            "LAMBDA,            21, public.ecr.aws/lambda/java:21"
    })
    void testDetermineBaseImage(String dockerBuildStrategy, String jdkVersion, String expectedImage) {
        String baseImage = JibMicronautExtension.determineBaseImage(jdkVersion, DockerBuildStrategy.valueOf(dockerBuildStrategy));
        assertEquals(expectedImage, baseImage);
    }
}
