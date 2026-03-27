package io.micronaut.maven;

import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MicronautRuntimeCompatibilityTest {

    @Test
    void supportsHttpServerJdkRuntimeGeneratedByStarter() {
        var runtime = MicronautRuntime.valueOf("http_server_jdk".toUpperCase(Locale.ROOT));

        assertEquals(MicronautRuntime.HTTP_SERVER_JDK, runtime);
        assertEquals(DockerBuildStrategy.DEFAULT, runtime.getBuildStrategy());
    }
}
