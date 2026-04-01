package io.micronaut.maven.aot;

import io.micronaut.maven.services.CompilerService;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AbstractMicronautAotMojoPackagingTest {

    @ParameterizedTest
    @CsvSource({
        "jkube-k8s, native, jit",
        "jkube-oc, native, jit",
        "docker-native, jit, native"
    })
    void normalizesAotRuntimeForPackaging(String packaging, String configuredRuntime, String expectedRuntime, @TempDir Path tempDir) throws MojoExecutionException {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getPackaging()).thenReturn(packaging);
        when(project.getBuild()).thenReturn(build);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());

        var mojo = new TestMicronautAotMojo(project, tempDir.resolve("classes").toFile());
        mojo.enabled = true;
        mojo.runtime = configuredRuntime;
        mojo.micronautAotVersion = "test";

        mojo.execute();

        assertEquals(expectedRuntime, mojo.runtime);
    }

    private static final class TestMicronautAotMojo extends AbstractMicronautAotMojo {

        private TestMicronautAotMojo(MavenProject mavenProject, File outputDirectory) {
            super(mock(CompilerService.class), mavenProject);
            this.outputDirectory = outputDirectory;
        }

        @Override
        void onSuccess(File outputDir) {
        }

        @Override
        protected void doExecute() {
        }

        @Override
        String getName() {
            return "test";
        }
    }
}
