package io.micronaut.maven.aot;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
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
            mock(CompilerService.class),
            mock(ExecutorService.class),
            mock(MavenProject.class),
            mock(DependencyResolutionService.class),
            mock(MavenSession.class),
            mock(ToolchainManager.class)
        );

        assertFalse(mojo.alignRuntimeWithPackaging());
    }
}
