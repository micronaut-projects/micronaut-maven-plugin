package io.micronaut.build.jib;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class JibMicronautExtensionTest {

    @ParameterizedTest
    @CsvSource({
            "1.8.0_352, latest",
            "10.0.1, latest",
            "11.0.6, jre11-latest",
            "14.0.2, jre11-latest",
            "17.0.1, jre17-latest",
            "17.0.4.1, jre17-latest",
            "19.0.1, jre17-latest"
    })
    void testDetermineJavaVersion(String javaVersion, String expectedFnVersion) {
        String fnVersion = JibMicronautExtension.determineProjectFnVersion(javaVersion);
        assertEquals(expectedFnVersion, fnVersion);
    }

    @ParameterizedTest
    @CsvSource({
            "latest, /usr/java/latest/bin/java -XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap -XX:-UsePerfData -XX:MaxRAMFraction=2 -XX:+UseSerialGC -Xshare:on -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint",
            "jre11-latest, /usr/java/latest/bin/java -XX:-UsePerfData -XX:+UseSerialGC -Xshare:on -Djava.awt.headless=true -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint",
            "jre17-latest, /usr/java/latest/bin/java -XX:-UsePerfData -XX:+UseSerialGC -Xshare:on -Djava.awt.headless=true -Djava.library.path=/function/runtime/lib -cp /function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/* com.fnproject.fn.runtime.EntryPoint"
    })
    void testBuildProjectFnEntrypoint(String projectFnVersion, String expectedEntrypoint) {
        String entrypoint = String.join(" ", JibMicronautExtension.buildProjectFnEntrypoint(projectFnVersion));
        assertEquals(expectedEntrypoint, entrypoint);
    }
}
