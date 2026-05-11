package io.micronaut.maven;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LifecycleMappingTest {

    @Test
    void k8sPackagingDelegatesPackageAndDeployLifecyclePhases() throws Exception {
        Element component = findLifecycleComponent("k8s");

        assertEquals(
            "org.eclipse.jkube:kubernetes-maven-plugin:build",
            phase(component, "package")
        );
        assertEquals(
            "org.eclipse.jkube:kubernetes-maven-plugin:push",
            phase(component, "deploy")
        );
    }

    @Test
    void openshiftPackagingDelegatesPackageAndDeployLifecyclePhases() throws Exception {
        Element component = findLifecycleComponent("openshift");

        assertEquals(
            "org.eclipse.jkube:openshift-maven-plugin:build",
            phase(component, "package")
        );
        assertEquals(
            "org.eclipse.jkube:openshift-maven-plugin:push",
            phase(component, "deploy")
        );
    }

    @Test
    void nativeImagePackagingRunsNativeImageJibAfterNativeCompile() throws Exception {
        Element component = findLifecycleComponent("native-image");

        String packagePhase = phase(component, "package");

        assertTrue(packagePhase.contains("org.graalvm.buildtools:native-maven-plugin:compile-no-fork"));
        assertTrue(packagePhase.contains(":native-image-jib"));
        assertTrue(packagePhase.indexOf("compile-no-fork") < packagePhase.indexOf("native-image-jib"));
        assertFalse(hasPhase(component, "deploy"));
    }

    private static Element findLifecycleComponent(String roleHint) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
        try (InputStream inputStream = LifecycleMappingTest.class.getClassLoader()
            .getResourceAsStream("META-INF/plexus/components.xml")) {
            assertNotNull(inputStream);
            var document = factory.newDocumentBuilder().parse(inputStream);
            var components = document.getElementsByTagName("component");
            for (int i = 0; i < components.getLength(); i++) {
                Element component = (Element) components.item(i);
                var hints = component.getElementsByTagName("role-hint");
                if (hints.getLength() == 1 && roleHint.equals(hints.item(0).getTextContent().trim())) {
                    return component;
                }
            }
        }
        throw new IllegalArgumentException("Lifecycle component not found: " + roleHint);
    }

    private static String phase(Element component, String phaseName) {
        Element phases = (Element) component.getElementsByTagName("phases").item(0);
        return Optional.ofNullable((Element) phases.getElementsByTagName(phaseName).item(0))
            .map(Element::getTextContent)
            .map(String::trim)
            .orElseThrow(() -> new IllegalArgumentException("Phase not found: " + phaseName));
    }

    private static boolean hasPhase(Element component, String phaseName) {
        Element phases = (Element) component.getElementsByTagName("phases").item(0);
        return phases.getElementsByTagName(phaseName).getLength() > 0;
    }
}
