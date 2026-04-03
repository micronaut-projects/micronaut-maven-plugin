package io.micronaut.maven;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PackagingTest {

    @ParameterizedTest
    @CsvSource({
        "k8s,K8S",
        "openshift,OPENSHIFT"
    })
    void supportsKubernetesAndOpenShiftPackagings(String value, Packaging expected) {
        assertEquals(expected, Packaging.of(value));
        assertEquals(value, expected.id());
    }
}
