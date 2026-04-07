package io.micronaut.maven.aot.internal;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.Locale;

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

    @Test
    void ofUsesRootLocaleForCaseConversion() {
        Locale previousDefault = Locale.getDefault();
        Locale.setDefault(Locale.forLanguageTag("tr"));
        try {
            assertEquals(AotPackaging.NATIVE_IMAGE, AotPackaging.of("native-image"));
        } finally {
            Locale.setDefault(previousDefault);
        }
    }
}
