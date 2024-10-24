package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import com.google.cloud.tools.jib.api.buildplan.Port;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import io.micronaut.maven.core.DockerBuildStrategy;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junitpioneer.jupiter.RestoreSystemProperties;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Paths;
import java.time.Instant;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
class JibMicronautExtensionTest {

    private static final Logger LOG = LoggerFactory.getLogger(JibMicronautExtensionTest.class);

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

    @Test
    @SetSystemProperty(key = "os.arch", value = "x64")
    void testDetectPlatforms() {
        var originalPlan = ContainerBuildPlan.builder().build();
        var finalPlan = extendContainerBuildPlan(originalPlan);

        assertEquals(1, finalPlan.getPlatforms().size());

        var platform = finalPlan.getPlatforms().iterator().next();
        assertEquals("amd64", platform.getArchitecture());
        assertEquals("linux", platform.getOs());
    }

    @Test
    void testConfigurePlatforms() {
        var originalPlan = ContainerBuildPlan.builder()
                .addPlatform("amd64", "linux")
                .addPlatform("arm64", "linux")
                .build();
        var finalPlan = extendContainerBuildPlan(originalPlan);

        assertEquals(2, finalPlan.getPlatforms().size());

        var iterator = finalPlan.getPlatforms().iterator();
        var platform = iterator.next();
        assertEquals("amd64", platform.getArchitecture());
        assertEquals("linux", platform.getOs());

        platform = iterator.next();
        assertEquals("arm64", platform.getArchitecture());
        assertEquals("linux", platform.getOs());
    }

    @Test
    void testDetectPorts() {
        var originalPlan = ContainerBuildPlan.builder().build();
        var finalPlan = extendContainerBuildPlan(originalPlan);

        assertEquals(1, finalPlan.getExposedPorts().size());
        assertTrue(finalPlan.getExposedPorts().contains(Port.tcp(8080)));
    }

    @Test
    void testConfigurePorts() {
        var originalPlan = ContainerBuildPlan.builder()
                .addExposedPort(Port.tcp(8080))
                .addExposedPort(Port.tcp(8081))
                .build();
        var finalPlan = extendContainerBuildPlan(originalPlan);

        assertEquals(2, finalPlan.getExposedPorts().size());
        assertTrue(finalPlan.getExposedPorts().contains(Port.tcp(8080)));
        assertTrue(finalPlan.getExposedPorts().contains(Port.tcp(8081)));
    }

    private ContainerBuildPlan extendContainerBuildPlan(ContainerBuildPlan originalPlan) {
        var extension = new JibMicronautExtension();
        var mavenData = new MavenData() {
            @Override
            public MavenProject getMavenProject() {
                var project = mock(MavenProject.class);
                when(project.getProperties()).thenReturn(new Properties());
                return project;
            }

            @Override
            public MavenSession getMavenSession() {
                return mock(MavenSession.class);
            }
        };
        ExtensionLogger extensionLogger = (logLevel, s) -> LOG.info(s);
        return extension.extendContainerBuildPlan(originalPlan, Map.of(), Optional.empty(), mavenData, extensionLogger);

    }
}
