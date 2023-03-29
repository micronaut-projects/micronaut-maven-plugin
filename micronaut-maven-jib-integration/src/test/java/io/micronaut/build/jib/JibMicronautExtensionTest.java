package io.micronaut.build.jib;

import io.micronaut.maven.jib.JibMicronautExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JibMicronautExtensionTest {

    @ParameterizedTest
    @CsvSource({
            "17.0.1, jre17-latest",
            "17.0.4.1, jre17-latest",
            "19.0.1, jre17-latest"
    })
    void testDetermineJavaVersion(String javaVersion, String expectedFnVersion) {
        String fnVersion = JibMicronautExtension.determineProjectFnVersion(javaVersion);
        assertEquals(expectedFnVersion, fnVersion);
    }

    @Test
    void testBuildProjectFnEntrypoint() {
        String entrypoint = String.join(" ", JibMicronautExtension.buildProjectFnEntrypoint());
        String expectedEntrypoint = "/usr/java/latest/bin/java -XX:-UsePerfData -XX:+UseSerialGC -Xshare:on -Djava.awt.headless=true -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint";
        assertEquals(expectedEntrypoint, entrypoint);
    }
}
