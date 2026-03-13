package io.micronaut.maven.jsonschema;

import io.micronaut.jsonschema.configuration.validator.DependencyInjectionError;
import io.micronaut.maven.services.CompilerService;
import org.apache.maven.model.Build;
import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AbstractConfigurationValidationMojoTest {

    @Test
    void buildDependencyInjectionSuppressionHintReturnsEmptyWhenNoCandidates() throws Exception {
        TestConfigurationValidationMojo mojo = new TestConfigurationValidationMojo();

        DependencyInjectionError error = mock(DependencyInjectionError.class);
        when(error.rootBean()).thenReturn("");
        when(error.bean()).thenReturn("  ");

        String hint = invokeSuppressionHint(mojo, Set.of(error));

        assertEquals("", hint);
    }

    @Test
    void buildDependencyInjectionSuppressionHintContainsSortedUniqueEntries() throws Exception {
        TestConfigurationValidationMojo mojo = new TestConfigurationValidationMojo();

        DependencyInjectionError first = mock(DependencyInjectionError.class);
        when(first.rootBean()).thenReturn("z.Bean");
        when(first.bean()).thenReturn("a.Bean");

        DependencyInjectionError second = mock(DependencyInjectionError.class);
        when(second.rootBean()).thenReturn("a.Bean");
        when(second.bean()).thenReturn("m.Bean");

        String hint = invokeSuppressionHint(mojo, Set.of(first, second));

        assertTrue(hint.contains("<configurationValidation>"));
        assertTrue(hint.contains("<suppressInjectError>a.Bean</suppressInjectError>"));
        assertTrue(hint.contains("<suppressInjectError>m.Bean</suppressInjectError>"));
        assertTrue(hint.contains("<suppressInjectError>z.Bean</suppressInjectError>"));
        assertTrue(hint.indexOf("a.Bean") < hint.indexOf("m.Bean"));
        assertTrue(hint.indexOf("m.Bean") < hint.indexOf("z.Bean"));
    }

    @Test
    void defaultPackageClasspathExcludesOutputDirectory(@TempDir Path tempDir) throws Exception {
        Path mainResources = tempDir.resolve("src/main/resources");
        Path outputDirectory = tempDir.resolve("target/classes");
        java.nio.file.Files.createDirectories(mainResources);
        java.nio.file.Files.createDirectories(outputDirectory);

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());

        Build build = new Build();
        build.setOutputDirectory(outputDirectory.toString());

        Resource resource = new Resource();
        resource.setDirectory(mainResources.toString());
        build.setResources(List.of(resource));
        build.setTestOutputDirectory(tempDir.resolve("target/test-classes").toString());
        project.setBuild(build);

        List<String> classpath = ConfigurationValidationClasspath.defaultPackageClasspath(project);

        assertEquals(List.of(outputDirectory.toString()), classpath);
    }

    @Test
    void defaultTestClasspathUsesCompiledOutputsWithoutDuplicatingMainResources(@TempDir Path tempDir) throws Exception {
        Path mainResources = tempDir.resolve("src/main/resources");
        Path outputDirectory = tempDir.resolve("target/classes");
        Path testOutputDirectory = tempDir.resolve("target/test-classes");
        java.nio.file.Files.createDirectories(mainResources);
        java.nio.file.Files.createDirectories(outputDirectory);
        java.nio.file.Files.createDirectories(testOutputDirectory);

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());

        Build build = new Build();
        build.setOutputDirectory(outputDirectory.toString());
        build.setTestOutputDirectory(testOutputDirectory.toString());

        Resource resource = new Resource();
        resource.setDirectory(mainResources.toString());
        build.setResources(List.of(resource));
        project.setBuild(build);

        List<String> classpath = ConfigurationValidationClasspath.defaultTestClasspath(project);

        assertEquals(List.of(outputDirectory.toString(), testOutputDirectory.toString()), classpath);
    }

    @Test
    void configurationValidationEnabledIsNullByDefault() {
        ConfigurationValidationConfiguration configuration = new ConfigurationValidationConfiguration();

        // By default, enabled is null (unset), not explicitly false
        assertNull(configuration.getEnabled());
    }

    @Test
    void configurationValidationEnabledReflectsExplicitlySetValue() {
        ConfigurationValidationConfiguration configuration = new ConfigurationValidationConfiguration();

        configuration.setEnabled(Boolean.TRUE);
        assertEquals(Boolean.TRUE, configuration.getEnabled());

        configuration.setEnabled(Boolean.FALSE);
        assertEquals(Boolean.FALSE, configuration.getEnabled());
    }

    private static String invokeSuppressionHint(AbstractConfigurationValidationMojo mojo,
                                                Set<DependencyInjectionError> errors) throws Exception {
        Method method = AbstractConfigurationValidationMojo.class
            .getDeclaredMethod("buildDependencyInjectionSuppressionHint", Set.class);
        method.setAccessible(true);
        return (String) method.invoke(mojo, errors);
    }


    private static final class TestConfigurationValidationMojo extends AbstractConfigurationValidationMojo {

        private TestConfigurationValidationMojo() {
            super(new MavenProject(), mock(CompilerService.class));
        }

        @Override
        protected String scenarioName() {
            return "test";
        }

        @Override
        protected ConfigurationValidationConfiguration.ValidationSet validationSet(ConfigurationValidationConfiguration cfg) {
            return null;
        }

        @Override
        protected List<String> defaultEnvironments() {
            return List.of();
        }

        @Override
        protected List<String> defaultClasspath(MavenProject project) {
            return List.of();
        }

        @Override
        protected String[] dependencyScopes() {
            return new String[0];
        }

        @Override
        protected List<Path> defaultResourceDirectories() {
            return List.of();
        }
    }
}
