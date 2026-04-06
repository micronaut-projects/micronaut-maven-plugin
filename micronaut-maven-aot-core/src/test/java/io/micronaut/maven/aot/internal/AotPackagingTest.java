package io.micronaut.maven.aot.internal;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AotPackagingTest {

    @ParameterizedTest
    @CsvSource({
        "k8s,K8S",
        "openshift,OPENSHIFT"
    })
    void supportsKubernetesAndOpenShiftPackagings(String value, AotPackaging expected) {
        assertEquals(expected, AotPackaging.of(value));
        assertEquals(value, expected.id());
    }

    @ParameterizedTest
    @CsvSource({
        "pom",
        "maven-plugin",
        "custom-packaging"
    })
    void findReturnsEmptyForUnsupportedPackagings(String value) {
        assertTrue(AotPackaging.find(value).isEmpty());
    }
}
