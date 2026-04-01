package io.micronaut.maven;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PackagingTest {

    @ParameterizedTest
    @CsvSource({
        "jar, JAR",
        "native-image, NATIVE_IMAGE",
        "docker, DOCKER",
        "jkube-k8s, JKUBE_K8S",
        "jkube-oc, JKUBE_OC",
        "docker-native, DOCKER_NATIVE",
        "docker-crac, DOCKER_CRAC"
    })
    void resolvesPackagingFromUserFacingIdentifiers(String value, Packaging expected) {
        assertEquals(expected, Packaging.of(value));
        assertEquals(value, expected.id());
    }
}
