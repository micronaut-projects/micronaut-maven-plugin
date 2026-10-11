package io.micronaut.maven.dev;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ControlPanelTest {

    @Test
    void addsTheBaseModulesToEveryRuntime() {
        assertEquals(List.of("micronaut-control-panel-ui", "micronaut-control-panel-management"),
            ControlPanel.modules(Set.of("io.micronaut:micronaut-http-server-netty")));
    }

    @Test
    void infersThePanelsOfTheModulesTheApplicationUses() {
        assertEquals(List.of("micronaut-control-panel-ui", "micronaut-control-panel-management", "micronaut-control-panel-kafka"),
            ControlPanel.modules(Set.of("io.micronaut.kafka:micronaut-kafka")));
        assertEquals(List.of(
                "micronaut-control-panel-ui",
                "micronaut-control-panel-management",
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
        assertEquals(2, ControlPanel.modules(Set.of("io.micronaut.kafka:micronaut-kafka-streams-only", "io.micronaut.cache:micronaut-cache-core")).size());
    }

    @Test
    void mirrorsPyronautsMapping() {
        assertEquals("micronaut-control-panel-datasource", ControlPanel.Feature.DATASOURCE.artifactId());
        assertEquals(List.of("io.micronaut.sql:micronaut-jdbc"), ControlPanel.Feature.DATASOURCE.triggers());
        assertEquals(List.of("org.hibernate.orm:hibernate-core"), ControlPanel.Feature.HIBERNATE.triggers());
        assertEquals(List.of("io.micronaut.kafka:micronaut-kafka"), ControlPanel.Feature.KAFKA.triggers());
        assertEquals(List.of("io.micronaut.objectstorage:micronaut-object-storage-core"), ControlPanel.Feature.OBJECT_STORAGE.triggers());
        assertEquals(4, ControlPanel.Feature.CACHE.triggers().size());
    }

    @Test
    void passesOnlyTheDefaultsTheApplicationDoesNotConfigure() {
        assertEquals(Map.of(
                "micronaut.control-panel.enabled", "true",
                "micronaut.control-panel.path", "/control-panel",
                "micronaut.control-panel.security.access", "ANONYMOUS"),
            ControlPanel.properties(key -> false));
        assertEquals(Map.of("micronaut.control-panel.enabled", "true", "micronaut.control-panel.security.access", "ANONYMOUS"),
            ControlPanel.properties(ControlPanel.PATH::equals));
    }

    @Test
    void readsSystemPropertiesAndEnvironmentVariables() {
        assertTrue(ControlPanel.isSystemProperty(ControlPanel.PATH, List.of("-Xmx1g", "-Dmicronaut.control-panel.path=/cp")));
        assertTrue(ControlPanel.isSystemProperty(ControlPanel.ENABLED, List.of("-Dmicronaut.control-panel.enabled")));
        assertFalse(ControlPanel.isSystemProperty(ControlPanel.PATH, List.of("-Dmicronaut.control-panel.path-other=x")));
        assertEquals("cloud", ControlPanel.systemProperty("micronaut.environments", List.of("-Dmicronaut.environments=test", "-Dmicronaut.environments=cloud")));
        assertEquals(null, ControlPanel.systemProperty("micronaut.environments", List.of("-Dother=x")));
        assertEquals("MICRONAUT_CONTROL_PANEL_SECURITY_ACCESS", ControlPanel.environmentVariable(ControlPanel.SECURITY_ACCESS));
    }

    @Test
    void readsTheKeysOfConfigurationFiles(@TempDir Path resources) throws IOException {
        Files.writeString(resources.resolve("application.properties"), "micronaut.control-panel.path=/cp\n");
        Files.writeString(resources.resolve("application-dev.yml"), """
            micronaut:
              controlPanel:
                security:
                  access: AUTHENTICATED
            """);
        assertEquals(Set.of(ControlPanel.PATH, ControlPanel.SECURITY_ACCESS), ControlPanel.configuredKeys(List.of(resources), ControlPanel.environments(null)));
    }

    @Test
    void readsFlatYamlAndJson(@TempDir Path resources) throws IOException {
        Files.writeString(resources.resolve("application.yml"), "micronaut.control-panel.enabled: false\n");
        Files.writeString(resources.resolve("application-local.json"), "{\"micronaut\": {\"control-panel\": {\"path\": \"/x\"}}}");
        assertEquals(Set.of(ControlPanel.ENABLED, ControlPanel.PATH), ControlPanel.configuredKeys(List.of(resources), ControlPanel.environments("local")));
    }

    @Test
    void ignoresTheFilesOfInactiveEnvironments(@TempDir Path resources) throws IOException {
        Files.writeString(resources.resolve("application-test.yml"), "micronaut.control-panel.enabled: false\n");
        Files.writeString(resources.resolve("application-dev.properties"), "micronaut.control-panel.path=/cp\n");
        assertEquals(Set.of(ControlPanel.PATH), ControlPanel.configuredKeys(List.of(resources), ControlPanel.environments(null)));
        assertEquals(Set.of(ControlPanel.ENABLED, ControlPanel.PATH), ControlPanel.configuredKeys(List.of(resources), ControlPanel.environments(" test , cloud")));
        assertEquals(Set.of("dev", "test", "cloud"), ControlPanel.environments(" test , cloud"));
    }

    @Test
    void ignoresOtherFilesAndMissingDirectories(@TempDir Path resources) throws IOException {
        Files.writeString(resources.resolve("logback.xml"), "<configuration>micronaut.control-panel.path</configuration>");
        Files.writeString(resources.resolve("bootstrap.yml"), "micronaut.control-panel.path: /x\n");
        Files.writeString(resources.resolve("application.yml"), "micronaut:\n  application:\n    name: demo\n");
        Files.writeString(resources.resolve("application-broken.yml"), "micronaut: [\n");
        assertEquals(Set.of(), ControlPanel.configuredKeys(List.of(resources, resources.resolve("missing")), ControlPanel.environments(null)));
    }

    @Test
    void readsOtherFormatsConservatively(@TempDir Path resources) throws IOException {
        Files.writeString(resources.resolve("application.toml"), "[micronaut.control-panel]\npath = \"/cp\"\n");
        assertEquals(Set.of(ControlPanel.PATH), ControlPanel.configuredKeys(List.of(resources), ControlPanel.environments(null)));
    }
}
