package io.micronaut.maven;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PackagingTest {

    @ParameterizedTest
    @CsvSource({
        "jkube-k8s,JKUBE_K8S",
        "jkube-oc,JKUBE_OC"
    })
    void supportsJkubePackagingAliases(String value, Packaging expected) {
        assertEquals(expected, Packaging.of(value));
        assertEquals(value, expected.id());
    }
}
