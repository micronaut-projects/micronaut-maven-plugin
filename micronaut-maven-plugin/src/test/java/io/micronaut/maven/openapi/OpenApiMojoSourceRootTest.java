package io.micronaut.maven.openapi;

import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Reproduces the source-root registration bug in {@link AbstractOpenApiMojo}.
 *
 * <p>The mojo passes {@code outputDirectory} to the generator via
 * {@code withOutputDirectory(outputDirectory)}, and the generator writes Java sources under
 * {@code outputDirectory/src/main/java}. However the mojo registers {@code outputDirectory}
 * itself (the base) as the compile source root, not the {@code src/main/java} sub-directory
 * where the files actually land. In reactor / incremental builds this causes the Micronaut
 * annotation processor to miss the generated types, so {@code $Introspection} classes are
 * never produced and downstream compilation fails with NoSuchFileException on the missing
 * {@code .class} files.
 */
class OpenApiMojoSourceRootTest {

    @ParameterizedTest
    @CsvSource({
        "java,src/main/java",
        "groovy,src/main/groovy"
    })
    void registersGeneratedSourcesUnderLanguageSourceFolder(String lang, String sourceFolder, @TempDir Path tmp) throws Exception {
        var project = mock(MavenProject.class);
        var mojo = new OpenApiServerMojo();

        File outputDirectory = tmp.resolve("generated-sources/openapi").toFile();

        setField(mojo, "project", project);
        setField(mojo, "outputDirectory", outputDirectory);
        setField(mojo, "enabled", true);
        setField(mojo, "lang", lang);

        // execute() registers the compile source root first, then runs the generator. The
        // generator fails here (no definition file configured), but the source root has
        // already been registered by that point, which is all this test asserts.
        try {
            mojo.execute();
        } catch (Throwable ignored) {
            // generation failure is irrelevant to source-root registration
        }

        var captor = ArgumentCaptor.forClass(String.class);
        verify(project, atLeastOnce()).addCompileSourceRoot(captor.capture());

        File expected = new File(outputDirectory, sourceFolder);
        assertTrue(
            captor.getAllValues().contains(expected.getAbsolutePath()),
            "Generated OpenAPI sources are written to " + expected
                + " but the registered compile source roots were " + captor.getAllValues()
                + ". The mojo must register the actual generated-sources directory (src/main/<lang>), "
                + "otherwise the Micronaut annotation processor never sees the generated types in "
                + "reactor/incremental builds (missing $Introspection classes).");
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        Class<?> c = target.getClass();
        while (c != null) {
            try {
                Field f = c.getDeclaredField(name);
                f.setAccessible(true);
                f.set(target, value);
                return;
            } catch (NoSuchFieldException e) {
                c = c.getSuperclass();
            }
        }
        throw new NoSuchFieldException(name);
    }
}
