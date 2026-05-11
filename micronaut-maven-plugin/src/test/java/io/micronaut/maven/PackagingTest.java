package io.micronaut.maven;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PackagingTest {

    @ParameterizedTest
    @CsvSource({
        "native-image-jib,NATIVE_IMAGE_JIB",
        "k8s,K8S",
        "openshift,OPENSHIFT"
    })
    void supportsKnownPackagings(String value, Packaging expected) {
        assertEquals(expected, Packaging.of(value));
        assertEquals(value, expected.id());
    }
}
