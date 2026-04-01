package io.micronaut.maven;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertTrue;

class LifecycleMappingTest {

    @Test
    void componentsXmlContainsJkubeLifecycleMappings() throws IOException {
        try (var stream = getClass().getResourceAsStream("/META-INF/plexus/components.xml")) {
            String componentsXml = new String(stream.readAllBytes(), StandardCharsets.UTF_8);

            assertTrue(componentsXml.contains("<role-hint>jkube-k8s</role-hint>"));
            assertTrue(componentsXml.contains("org.eclipse.jkube:kubernetes-maven-plugin:build"));
            assertTrue(componentsXml.contains("org.eclipse.jkube:kubernetes-maven-plugin:push"));
            assertTrue(componentsXml.contains("<role-hint>jkube-oc</role-hint>"));
            assertTrue(componentsXml.contains("org.eclipse.jkube:openshift-maven-plugin:build"));
            assertTrue(componentsXml.contains("org.eclipse.jkube:openshift-maven-plugin:push"));
        }
    }
}
