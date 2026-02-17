package io.micronaut.maven.testresources;

import io.micronaut.maven.services.DependencyResolutionService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Properties;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Unit test for StartTestResourcesServerMojo.
 */
class StartTestResourcesServerMojoTest {

    @Mock
    private MavenProject mavenProject;

    @Mock
    private MavenSession mavenSession;

    @Mock
    private DependencyResolutionService dependencyResolutionService;

    @Mock
    private ToolchainManager toolchainManager;

    @Mock
    private Plugin plugin;

    private StartTestResourcesServerMojo mojo;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mojo = new StartTestResourcesServerMojo(mavenProject, mavenSession, dependencyResolutionService, toolchainManager);
        
        when(mavenProject.getPlugin("io.micronaut.maven:micronaut-maven-plugin")).thenReturn(plugin);
        when(mavenSession.getUserProperties()).thenReturn(new Properties());
        when(mavenSession.getSystemProperties()).thenReturn(new Properties());
    }

    @Test
    void shouldSkipWhenSkipPropertyIsTrue() {
        // Given
        Properties userProps = new Properties();
        userProps.setProperty("skipITs", "true");
        when(mavenSession.getUserProperties()).thenReturn(userProps);

        // When
        boolean result = mojo.isTestExecutionSkipped();

        // Then
        assertTrue(result, "Should skip when skipITs is true");
    }

    @Test
    void shouldNotSkipWhenNoSkipPropertiesSet() {
        // Given - properties already set to empty in setUp()

        // When
        boolean result = mojo.isTestExecutionSkipped();

        // Then
        assertFalse(result, "Should not skip when no skip properties are set");
    }

    @Test
    void shouldNotSkipWhenSkipPropertyIsFalse() {
        // Given
        Properties userProps = new Properties();
        userProps.setProperty("skipITs", "false");
        when(mavenSession.getUserProperties()).thenReturn(userProps);

        // When
        boolean result = mojo.isTestExecutionSkipped();

        // Then
        assertFalse(result, "Should not skip when skip properties are false");
    }

    @Test
    void shouldExecuteWithoutExceptionWhenTestsAreNotSkipped() throws MojoExecutionException {
        // Given
        mojo.setTestResourcesEnabled(false); // Disable to avoid actual server start

        // When/Then - should not throw exception
        assertDoesNotThrow(() -> mojo.execute());
    }
}