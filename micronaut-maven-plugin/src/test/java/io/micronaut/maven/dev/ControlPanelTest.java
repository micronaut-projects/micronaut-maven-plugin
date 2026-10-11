package io.micronaut.maven.dev;

import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.collection.DependencyCollectionException;
import org.eclipse.aether.collection.CollectResult;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.transfer.ArtifactNotFoundException;
import org.eclipse.aether.transfer.ArtifactTransferException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ControlPanelTest {

    @Test
    void addsTheBaseModulesToEveryRuntime() {
        assertEquals(List.of("micronaut-control-panel-ui", "micronaut-control-panel-management", "micronaut-control-panel-testing"),
            ControlPanel.modules(Set.of("io.micronaut:micronaut-http-server-netty")));
    }

    @Test
    void infersThePanelsOfTheModulesTheApplicationUses() {
        assertEquals(List.of("micronaut-control-panel-ui", "micronaut-control-panel-management", "micronaut-control-panel-testing", "micronaut-control-panel-kafka"),
            ControlPanel.modules(Set.of("io.micronaut.kafka:micronaut-kafka")));
        assertEquals(List.of(
                "micronaut-control-panel-ui",
                "micronaut-control-panel-management",
                "micronaut-control-panel-testing",
                "micronaut-control-panel-datasource",
                "micronaut-control-panel-hibernate",
                "micronaut-control-panel-object-storage",
                "micronaut-control-panel-cache"),
            ControlPanel.modules(Set.of(
                "io.micronaut.sql:micronaut-jdbc",
                "org.hibernate.orm:hibernate-core",
                "io.micronaut.objectstorage:micronaut-object-storage-core",
                "io.micronaut.cache:micronaut-cache-ehcache")));
        // a module of the same group that the panel does not inspect adds nothing
        assertEquals(3, ControlPanel.modules(Set.of("io.micronaut.kafka:micronaut-kafka-streams", "io.micronaut.cache:micronaut-cache-core")).size());
    }

    @Test
    void mirrorsPyronautsMapping() {
        assertEquals("micronaut-control-panel-datasource", ControlPanel.Feature.DATASOURCE.artifactId());
        assertEquals(List.of("io.micronaut.sql:micronaut-jdbc"), ControlPanel.Feature.DATASOURCE.triggers());
        assertEquals(List.of("org.hibernate.orm:hibernate-core"), ControlPanel.Feature.HIBERNATE.triggers());
        assertEquals(List.of("io.micronaut.kafka:micronaut-kafka"), ControlPanel.Feature.KAFKA.triggers());
        assertEquals(List.of("io.micronaut.objectstorage:micronaut-object-storage-core"), ControlPanel.Feature.OBJECT_STORAGE.triggers());
        assertEquals(List.of(
            "io.micronaut.cache:micronaut-cache-caffeine",
            "io.micronaut.cache:micronaut-cache-ehcache",
            "io.micronaut.cache:micronaut-cache-hazelcast",
            "io.micronaut.cache:micronaut-cache-infinispan"), ControlPanel.Feature.CACHE.triggers());
    }

    @Test
    void writesTheSettingsAsTheResourceControlPanelReadsAtTheLowestPrecedence(@TempDir Path directory) throws IOException {
        Path resources = directory.resolve("control-panel");
        ControlPanel.writeResources(resources, Set.of("io.micronaut:micronaut-http-server-netty"));
        assertEquals(Map.of(
            "micronaut.control-panel.enabled", "true",
            "micronaut.control-panel.path", "/control-panel",
            "micronaut.control-panel.security.access", "ANONYMOUS"), read(resources));

        ControlPanel.deleteResources(resources);
        assertFalse(Files.exists(resources));
        // nothing to delete
        ControlPanel.deleteResources(resources);
    }

    @Test
    void leavesTheAccessToTheApplicationsSecurityWhenMicronautSecurityIsOnTheClasspath(@TempDir Path directory) throws IOException {
        Path resources = directory.resolve("control-panel");
        ControlPanel.writeResources(resources, Set.of("io.micronaut:micronaut-http-server-netty", "io.micronaut.security:micronaut-security"));
        Map<String, String> properties = read(resources);
        assertEquals(Map.of(
            "micronaut.control-panel.enabled", "true",
            "micronaut.control-panel.path", "/control-panel"), properties);
        assertFalse(properties.containsKey("micronaut.control-panel.security.access"));
        // a module of the same group that is not Micronaut Security changes nothing
        assertEquals(ControlPanel.DEFAULT_PROPERTIES, ControlPanel.properties(Set.of("io.micronaut.security:micronaut-security-annotations")));
    }

    @Test
    void toleratesOnlyMissingControlPanelArtifacts() {
        assertTrue(ControlPanel.isMissingControlPanelArtifact(failure(notFound("io.micronaut.controlpanel", "micronaut-control-panel-testing"))));
        assertFalse(ControlPanel.isMissingControlPanelArtifact(failure(notFound("io.micronaut", "micronaut-management"))));
        assertFalse(ControlPanel.isMissingControlPanelArtifact(failure(notFound("io.micronaut.controlpanel", "micronaut-control-panel-testing"), notFound("io.micronaut", "micronaut-management"))));
        // a repository that cannot be reached is not a module that is not published
        DefaultArtifact artifact = new DefaultArtifact("io.micronaut.controlpanel:micronaut-control-panel-ui:2.2.0");
        ArtifactResult unreachable = new ArtifactResult(new ArtifactRequest(artifact, List.of(), null));
        unreachable.addException(new ArtifactTransferException(artifact, null, "Connection refused"));
        assertFalse(ControlPanel.isMissingControlPanelArtifact(failure(unreachable)));
        assertFalse(ControlPanel.isMissingControlPanelArtifact(new IllegalStateException("other")));
    }

    @Test
    void toleratesAMissingDescriptorDuringCollection() {
        ArtifactResolutionException missingPom = new ArtifactResolutionException(List.of(notFound("io.micronaut.controlpanel", "micronaut-control-panel-testing")));
        DependencyCollectionException collection = new DependencyCollectionException(new CollectResult(new CollectRequest()), "Cannot collect", missingPom);
        DependencyResolutionException failure = new DependencyResolutionException(new DependencyResult(new DependencyRequest()), collection);
        assertTrue(ControlPanel.isMissingControlPanelArtifact(failure));
    }

    private static Map<String, String> read(Path resources) throws IOException {
        Properties properties = new Properties();
        try (Reader reader = Files.newBufferedReader(resources.resolve("micronaut-control-panel/micronaut-control-panel.properties"))) {
            properties.load(reader);
        }
        Map<String, String> result = new java.util.HashMap<>();
        properties.stringPropertyNames().forEach(name -> result.put(name, properties.getProperty(name)));
        return result;
    }

    private static ArtifactResult notFound(String group, String artifactId) {
        DefaultArtifact artifact = new DefaultArtifact(group + ":" + artifactId + ":2.2.0");
        ArtifactResult result = new ArtifactResult(new ArtifactRequest(artifact, List.of(), null));
        result.addException(new ArtifactNotFoundException(artifact, null));
        return result;
    }

    private static DependencyResolutionException failure(ArtifactResult... results) {
        ArtifactResolutionException resolution = new ArtifactResolutionException(List.of(results));
        return new DependencyResolutionException(new DependencyResult(new DependencyRequest()), resolution);
    }
}
