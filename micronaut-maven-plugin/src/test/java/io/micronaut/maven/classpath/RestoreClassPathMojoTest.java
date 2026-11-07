package io.micronaut.maven.classpath;

import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class RestoreClassPathMojoTest {

    @Test
    void restoresTheSwappedDependencies() {
        var project = mock(MavenProject.class);
        var rewritten = mock(RewrittenClassPath.class);
        when(project.getContextValue(TransformClassPathMojo.CONTEXT_KEY)).thenReturn(rewritten);

        new RestoreClassPathMojo(project).execute();

        verify(rewritten).close();
        verify(project).setContextValue(TransformClassPathMojo.CONTEXT_KEY, null);
    }

    @Test
    void doesNothingWithoutARewrittenClassPath() {
        var project = mock(MavenProject.class);

        new RestoreClassPathMojo(project).execute();

        verify(project, never()).setContextValue(eq(TransformClassPathMojo.CONTEXT_KEY), any());
    }
}
