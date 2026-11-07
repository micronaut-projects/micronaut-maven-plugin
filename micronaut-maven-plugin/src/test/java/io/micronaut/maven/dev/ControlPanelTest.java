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

import java.io.File;
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
    void requiresMicronautPlatform524OrLater() {
        for (String version : List.of("5.2.4", "5.2.10", "5.3.0", "5.3.0-M1", "6.0.0", "5.2.4.1")) {
            assertTrue(ControlPanel.isSupportedPlatform(version), version);
        }
        for (String version : List.of("5.2.3", "5.2.4-SNAPSHOT", "5.2.4-RC1", "5.1.9", "4.10.0", "", " ")) {
            assertFalse(ControlPanel.isSupportedPlatform(version), version);
        }
        assertFalse(ControlPanel.isSupportedPlatform(null));
    }

    @Test
    void findsThePlatformVersionInTheDependencyManagementOrAmongTheParents() {
        org.apache.maven.project.MavenProject parent = new org.apache.maven.project.MavenProject();
        parent.setGroupId("io.micronaut.platform");
        parent.setArtifactId("micronaut-parent");
        parent.setVersion("5.2.4");
        org.apache.maven.project.MavenProject child = new org.apache.maven.project.MavenProject();
        child.setGroupId("example");
        child.setArtifactId("app");
        child.setVersion("0.1");
        child.setParent(parent);
        assertEquals(java.util.Optional.of("5.2.4"), ControlPanel.platformVersion(child));

        // a project that imports the BOM: the effective dependency management holds micronaut-parent at its version
        org.apache.maven.model.Dependency managed = new org.apache.maven.model.Dependency();
        managed.setGroupId("io.micronaut.platform");
        managed.setArtifactId("micronaut-parent");
        managed.setVersion("5.2.3");
        managed.setType("pom");
        org.apache.maven.model.DependencyManagement management = new org.apache.maven.model.DependencyManagement();
        management.addDependency(managed);
        org.apache.maven.model.Model model = new org.apache.maven.model.Model();
        model.setDependencyManagement(management);
        assertEquals(java.util.Optional.of("5.2.3"), ControlPanel.platformVersion(new org.apache.maven.project.MavenProject(model)));

        assertEquals(java.util.Optional.empty(), ControlPanel.platformVersion(new org.apache.maven.project.MavenProject()));

        // a dependency management that does not version the platform leaves it to the parents
        org.apache.maven.model.Dependency other = new org.apache.maven.model.Dependency();
        other.setGroupId("io.micronaut.platform");
        other.setArtifactId("micronaut-other");
        other.setVersion("9.9.9");
        org.apache.maven.model.Dependency unversioned = new org.apache.maven.model.Dependency();
        unversioned.setGroupId("io.micronaut.platform");
        unversioned.setArtifactId("micronaut-parent");
        org.apache.maven.model.DependencyManagement others = new org.apache.maven.model.DependencyManagement();
        others.addDependency(other);
        others.addDependency(unversioned);
        child.getModel().setDependencyManagement(others);
        assertEquals(java.util.Optional.of("5.2.4"), ControlPanel.platformVersion(child));
    }

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
        // a failure that names no missing artifact says nothing about what is published
        assertFalse(ControlPanel.isMissingControlPanelArtifact(failure()));
        assertFalse(ControlPanel.isMissingControlPanelArtifact(failure(new ArtifactResult(new ArtifactRequest(artifact, List.of(), null)))));
    }

    @Test
    void toleratesAMissingDescriptorDuringCollection() {
        ArtifactResolutionException missingPom = new ArtifactResolutionException(List.of(notFound("io.micronaut.controlpanel", "micronaut-control-panel-testing")));
        DependencyCollectionException collection = new DependencyCollectionException(new CollectResult(new CollectRequest()), "Cannot collect", missingPom);
        DependencyResolutionException failure = new DependencyResolutionException(new DependencyResult(new DependencyRequest()), collection);
        assertTrue(ControlPanel.isMissingControlPanelArtifact(failure));
    }

    @Test
    void addsWhatARuntimeClasspathDoesNotHoldOnceInTheOrderItResolved() {
        File ui = new File("ui.jar");
        File shared = new File("shared.jar");
        File management = new File("management.jar");
        ControlPanel.Additions additions = new ControlPanel.Additions(Set.of("example:runtime"));

        assertTrue(additions.add("micronaut-control-panel-ui", List.of(
            resolved(ControlPanel.GROUP, "micronaut-control-panel-ui", ui),
            resolved("example", "shared", shared),
            resolved("example", "runtime", new File("other-runtime.jar")))));
        assertTrue(additions.add("micronaut-control-panel-management", List.of(
            resolved("example", "shared", new File("other-shared.jar")),
            resolved(ControlPanel.GROUP, "micronaut-control-panel-management", management))));

        assertEquals(List.of(ui, shared, management), additions.files());
        assertEquals(List.of("micronaut-control-panel-ui", "micronaut-control-panel-management"), additions.added());
        assertTrue(additions.active());
    }

    @Test
    void aModuleThatIsNotAmongItsResultsIsNotAdded() {
        File transitive = new File("transitive.jar");
        ControlPanel.Additions additions = new ControlPanel.Additions(Set.of());
        ArtifactResult withoutFile = new ArtifactResult(new ArtifactRequest());
        withoutFile.setArtifact(new DefaultArtifact(ControlPanel.GROUP, "micronaut-control-panel-ui", "jar", "2.2.0"));

        assertFalse(additions.add("micronaut-control-panel-ui", List.of()));
        assertFalse(additions.add("micronaut-control-panel-ui", List.of(new ArtifactResult(new ArtifactRequest()), withoutFile)));
        assertFalse(additions.active());

        assertFalse(additions.add("micronaut-control-panel-ui", List.of(resolved("example", "transitive", transitive))));
        assertEquals(List.of(transitive), additions.files());
        assertEquals(List.of(), additions.added());
        assertFalse(additions.active());
    }

    @Test
    void aModuleTheRuntimeClasspathHoldsMakesItActiveWithoutAddingAnything() {
        ControlPanel.Additions additions = new ControlPanel.Additions(Set.of("io.micronaut.controlpanel:micronaut-control-panel-ui"));

        assertFalse(additions.holds("micronaut-control-panel-management"));
        assertFalse(additions.active());
        assertTrue(additions.holds("micronaut-control-panel-ui"));
        assertTrue(additions.active());
        assertEquals(List.of(), additions.files());
        assertEquals(List.of(), additions.added());
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

    private static ArtifactResult resolved(String group, String artifactId, File file) {
        ArtifactResult result = new ArtifactResult(new ArtifactRequest());
        result.setArtifact(new DefaultArtifact(group, artifactId, "jar", "2.2.0").setFile(file));
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
