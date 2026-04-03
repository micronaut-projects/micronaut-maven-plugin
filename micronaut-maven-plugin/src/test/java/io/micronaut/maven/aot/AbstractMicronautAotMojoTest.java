package io.micronaut.maven.aot;

import io.micronaut.maven.aot.internal.AotCompilerService;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AbstractMicronautAotMojoTest {

    @ParameterizedTest
    @ValueSource(strings = {"k8s", "openshift"})
    void treatsKubernetesAndOpenShiftPackagingsAsJitRuntime(String packaging, @TempDir Path tempDir) throws Exception {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getPackaging()).thenReturn(packaging);
        when(project.getBuild()).thenReturn(build);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());

        var mojo = new TestMicronautAotMojo(project);
        mojo.enabled = true;
        mojo.runtime = "native";
        mojo.micronautAotVersion = "test";
        mojo.outputDirectory = tempDir.resolve("classes").toFile();

        mojo.execute();

        assertEquals("jit", mojo.runtime);
    }

    private static final class TestMicronautAotMojo extends AbstractMicronautAotMojo {

        private TestMicronautAotMojo(MavenProject mavenProject) {
            super(mock(AotCompilerService.class), mavenProject);
        }

        @Override
        void onSuccess(File outputDir) {
            // This test only verifies runtime coercion before the success hook runs.
        }

        @Override
        protected void doExecute() throws DependencyResolutionException, MojoExecutionException {
            // No-op: the test exercises execute() up to runtime normalization only.
        }

        @Override
        String getName() {
            return "test";
        }
    }
}
