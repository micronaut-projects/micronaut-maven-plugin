package io.micronaut.maven.aot;

import io.micronaut.maven.aot.internal.AotCompilerService;
import io.micronaut.maven.aot.internal.AotDependencyResolutionService;
import io.micronaut.maven.aot.internal.AotExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;

class AotSampleMojoTest {

    @Test
    void standalonePluginDoesNotAlignRuntimeWithPackaging() {
        AotSampleMojo mojo = new AotSampleMojo(
            mock(AotCompilerService.class),
            mock(AotExecutorService.class),
            mock(MavenProject.class),
            mock(AotDependencyResolutionService.class),
            mock(MavenSession.class),
            mock(ToolchainManager.class)
        );

        assertFalse(mojo.alignRuntimeWithPackaging());
    }
}
