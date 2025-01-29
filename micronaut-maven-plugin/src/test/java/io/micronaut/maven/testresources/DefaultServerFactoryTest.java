package io.micronaut.maven.testresources;

import io.micronaut.maven.MojoUtils;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class DefaultServerFactoryTest {

    @Test
    void itPassesSystemProperties() {
        try (MockedStatic<MojoUtils> mojoUtils = mockStatic(MojoUtils.class)) {
            var serverFactory = new DefaultServerFactory(null, null, null, null, null, false, false, Map.of("foo", "bar"));
            mojoUtils.when(() -> MojoUtils.findJavaExecutable(any(), any())).thenReturn("java");
            var cliArguments = serverFactory.computeCliArguments(createProcessParameters());
            assertTrue(cliArguments.contains("-Dfoo=bar"));
        }
    }

    private ServerUtils.ProcessParameters createProcessParameters() {
        var processParameters = mock(ServerUtils.ProcessParameters.class);
        when(processParameters.getMainClass()).thenReturn("com.example.MyApp");
        return processParameters;
    }
}
