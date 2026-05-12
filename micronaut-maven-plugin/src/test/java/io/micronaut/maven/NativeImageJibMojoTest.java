package io.micronaut.maven;

import com.google.cloud.tools.jib.api.buildplan.Platform;
import io.micronaut.maven.jib.JibConfiguration;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junitpioneer.jupiter.RestoreSystemProperties;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
class NativeImageJibMojoTest {

    @Test
    void createsContainerPlanWithOnlyNativeExecutableLayer(@TempDir Path tempDir) throws Exception {
        Path executable = Files.writeString(tempDir.resolve("demo"), "native");
        var mojo = newMojo(tempDir);

        var plan = mojo.createContainerBuilder(executable, new Platform("amd64", "linux")).toContainerBuildPlan();

        assertEquals(List.of("/app/demo"), plan.getEntrypoint());
        assertEquals("/app", plan.getWorkingDirectory().toString());
        assertEquals("65532", plan.getUser());
        assertEquals(1, plan.getPlatforms().size());
        assertEquals("amd64", plan.getPlatforms().iterator().next().getArchitecture());
        assertEquals(1, plan.getLayers().size());
        var layer = (com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer) plan.getLayers().get(0);
        assertEquals(1, layer.getEntries().size());
        var entry = layer.getEntries().get(0);
        assertEquals(executable, entry.getSourceFile());
        assertEquals("/app/demo", entry.getExtractionPath().toString());
        assertEquals("755", entry.getPermissions().toOctalString());
    }

    @Test
    void buildsTarContainerFromNativeExecutable(@TempDir Path tempDir) throws Exception {
        Files.createDirectories(tempDir.resolve("target"));
        Files.writeString(tempDir.resolve("target/demo"), "native");
        Path tarball = tempDir.resolve("target/native-image.tar");
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;
        when(mojo.jibConfigurationService.getFromImage()).thenReturn(Optional.of("scratch"));
        when(mojo.jibConfigurationService.getToImage()).thenReturn(Optional.of("example.com/micronaut/demo:0.1"));
        when(mojo.jibConfigurationService.getTags()).thenReturn(Set.of("latest", "ci"));
        when(mojo.jibConfigurationService.getOutputPathsTar()).thenReturn(Optional.of(tarball.toString()));

        mojo.execute();

        assertEquals("buildTar", mojo.jibBuildGoal);
        assertTrue(Files.exists(tarball));
    }

    @Test
    void blankTarOutputPathFallsBackToDefault(@TempDir Path tempDir) throws Exception {
        Files.createDirectories(tempDir.resolve("target"));
        Files.writeString(tempDir.resolve("target/demo"), "native");
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;
        when(mojo.jibConfigurationService.getFromImage()).thenReturn(Optional.of("scratch"));
        when(mojo.jibConfigurationService.getToImage()).thenReturn(Optional.of("example.com/micronaut/demo:0.1"));
        when(mojo.jibConfigurationService.getOutputPathsTar()).thenReturn(Optional.of(" "));

        mojo.execute();

        assertTrue(Files.exists(tempDir.resolve("target/jib-image.tar")));
    }

    @Test
    void appliesConfiguredContainerSettings(@TempDir Path tempDir) throws Exception {
        Path executable = Files.writeString(tempDir.resolve("demo"), "native");
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getEntrypoint()).thenReturn(List.of("/custom-entrypoint"));
        when(mojo.jibConfigurationService.getArgs()).thenReturn(List.of("--debug"));
        when(mojo.jibConfigurationService.getUser()).thenReturn(Optional.of("1001"));

        var plan = mojo.createContainerBuilder(executable, new Platform("arm64", "linux")).toContainerBuildPlan();

        assertEquals(List.of("/custom-entrypoint"), plan.getEntrypoint());
        assertEquals(List.of("--debug"), plan.getCmd());
        assertEquals("1001", plan.getUser());
        assertEquals("arm64", plan.getPlatforms().iterator().next().getArchitecture());
    }

    @Test
    void usesNoExposedPortsWhenConfiguredPortsAreBlank(@TempDir Path tempDir) throws Exception {
        Path executable = Files.writeString(tempDir.resolve("demo"), "native");
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getPorts()).thenReturn(Optional.of(" "));

        var plan = mojo.createContainerBuilder(executable, new Platform("amd64", "linux")).toContainerBuildPlan();

        assertTrue(plan.getExposedPorts().isEmpty());
    }

    @Test
    void rejectsInvalidConfiguredPort(@TempDir Path tempDir) throws Exception {
        Path executable = Files.writeString(tempDir.resolve("demo"), "native");
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getPorts()).thenReturn(Optional.of("8080/http"));

        var exception = assertThrows(MojoExecutionException.class,
            () -> mojo.createContainerBuilder(executable, new Platform("amd64", "linux")));

        assertEquals("jib.container.ports contains an invalid exposed port token: 8080/http", exception.getMessage());
    }

    @Test
    void skipsWhenNativeImageJibIsNotEnabled(@TempDir Path tempDir) throws Exception {
        var mojo = newMojo(tempDir);
        mojo.enabled = false;

        mojo.execute();

        assertEquals("dockerBuild", mojo.jibBuildGoal);
    }

    @Test
    void rejectsDockerBackedJibGoal(@TempDir Path tempDir) {
        Properties userProperties = new Properties();
        userProperties.setProperty("jib.buildGoal", "dockerBuild");
        var mojo = newMojo(tempDir, userProperties, new Properties());
        mojo.jibBuildGoal = "dockerBuild";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Unsupported jib.buildGoal 'dockerBuild' for native image Jib packaging. Supported values are: buildTar, build. Use docker-native packaging for Docker-backed native image builds.", exception.getMessage());
    }

    @Test
    void rejectsDockerBackedJibGoalConfiguredOnMojo(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir, new Properties(), new Properties(),
            mockMojoExecution(mojoConfiguration("jibBuildGoal", "dockerBuild")));
        mojo.jibBuildGoal = "dockerBuild";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Unsupported jib.buildGoal 'dockerBuild' for native image Jib packaging. Supported values are: buildTar, build. Use docker-native packaging for Docker-backed native image builds.", exception.getMessage());
    }

    @Test
    void defaultsToBuildTarWhenJibBuildGoalIsNotConfigured(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("buildTar", mojo.jibBuildGoal);
        assertTrue(exception.getMessage().contains("Native executable not found:"));
    }

    @Test
    void defaultsToBuildTarWhenMojoConfigurationOnlyContainsDescriptorDefault(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir, new Properties(), new Properties(),
            mockMojoExecution(descriptorDefaultMojoConfiguration()));
        mojo.allowPlatformMismatch = true;
        mojo.jibBuildGoal = "dockerBuild";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("buildTar", mojo.jibBuildGoal);
        assertTrue(exception.getMessage().contains("Native executable not found:"));
    }

    @Test
    void failsWhenNativeExecutableIsMissing(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Native executable not found:"));
        assertTrue(exception.getMessage().replace('\\', '/').contains("target/demo"));
    }

    @Test
    void usesNativeImagePluginImageNameForDefaultExecutable(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;
        when(mojo.mavenProject.getPlugin("org.graalvm.buildtools:native-maven-plugin")).thenReturn(nativeImagePlugin("custom-demo"));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("Native executable not found:"));
        assertTrue(exception.getMessage().replace('\\', '/').contains("target/custom-demo"));
    }

    @Test
    void rejectsMultipleTargetPlatforms(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.of("amd64"), Optional.of("linux")),
            new JibConfiguration.PlatformConfiguration(Optional.of("arm64"), Optional.of("linux"))
        ));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("native image Jib supports exactly one target platform because it packages one local native executable.", exception.getMessage());
    }

    @Test
    void rejectsConfiguredPlatformWithoutArchitecture(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.empty(), Optional.of("linux"))
        ));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("jib.from.platforms must define an architecture for native image Jib packaging.", exception.getMessage());
    }

    @Test
    void rejectsNonLinuxTargetPlatform(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.of("amd64"), Optional.of("windows"))
        ));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("native image Jib packages Linux container images only. Configured platform is windows/amd64.", exception.getMessage());
    }

    @Test
    void rejectsHostArchitectureMismatchWithoutOverride(@TempDir Path tempDir) {
        System.setProperty("os.name", "Linux");
        System.setProperty("os.arch", "amd64");
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.of("arm64"), Optional.of("linux"))
        ));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("native image Jib host architecture amd64 does not match configured target architecture arm64. Set -D"
            + NativeImageJibMojo.ALLOW_PLATFORM_MISMATCH_PROPERTY + "=true only for a known compatible cross-compiled executable.", exception.getMessage());
    }

    @Test
    void rejectsInvalidAdditionalTag(@TempDir Path tempDir) throws Exception {
        Files.createDirectories(tempDir.resolve("target"));
        Files.writeString(tempDir.resolve("target/demo"), "native");
        var mojo = newMojo(tempDir);
        mojo.allowPlatformMismatch = true;
        when(mojo.jibConfigurationService.getFromImage()).thenReturn(Optional.of("scratch"));
        when(mojo.jibConfigurationService.getToImage()).thenReturn(Optional.of("example.com/micronaut/demo:0.1"));
        when(mojo.jibConfigurationService.getTags()).thenReturn(Set.of("bad tag"));

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("jib.to.tags contains an invalid image tag for native image Jib: bad tag", exception.getMessage());
    }

    @Test
    void rejectsInvalidBaseImage(@TempDir Path tempDir) throws Exception {
        Path executable = Files.writeString(tempDir.resolve("demo"), "native");
        var mojo = newMojo(tempDir);
        when(mojo.jibConfigurationService.getFromImage()).thenReturn(Optional.of("bad image"));

        var exception = assertThrows(MojoExecutionException.class,
            () -> mojo.createContainerBuilder(executable, new Platform("amd64", "linux")));

        assertTrue(exception.getMessage().contains("native image Jib base image is not a valid Docker image reference: bad image"));
    }

    @Test
    void rejectsNonLinuxHostWithoutOverride(@TempDir Path tempDir) {
        System.setProperty("os.name", "Mac OS X");
        var mojo = newMojo(tempDir);

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(exception.getMessage().contains("requires a Linux host by default"));
        assertTrue(exception.getMessage().contains(NativeImageJibMojo.ALLOW_PLATFORM_MISMATCH_PROPERTY));
    }

    @Test
    void rejectsLambdaRuntime(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        mojo.micronautRuntime = "lambda";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("native image Jib packaging does not support micronaut.runtime=lambda. Use docker-native packaging for Lambda and Oracle Function native images.", exception.getMessage());
    }

    @Test
    void rejectsInvalidRuntimeWithMojoExecutionException(@TempDir Path tempDir) {
        var mojo = newMojo(tempDir);
        mojo.micronautRuntime = "invalid";

        var exception = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Unsupported micronaut.runtime 'invalid' for native image Jib packaging.", exception.getMessage());
        assertTrue(exception.getCause() instanceof IllegalArgumentException);
    }

    private static NativeImageJibMojo newMojo(Path tempDir) {
        return newMojo(tempDir, new Properties(), new Properties());
    }

    private static NativeImageJibMojo newMojo(Path tempDir, Properties userProperties, Properties systemProperties) {
        return newMojo(tempDir, userProperties, systemProperties, mockMojoExecution(null));
    }

    private static NativeImageJibMojo newMojo(Path tempDir, Properties userProperties, Properties systemProperties,
                                             MojoExecution mojoExecution) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        when(jibConfigurationService.getFromCredentials()).thenReturn(Optional.empty());
        when(jibConfigurationService.getToCredentials()).thenReturn(Optional.empty());
        when(jibConfigurationService.getToImage()).thenReturn(Optional.empty());
        when(jibConfigurationService.getTags()).thenReturn(Collections.emptySet());
        when(jibConfigurationService.getArgs()).thenReturn(Collections.emptyList());
        when(jibConfigurationService.getEntrypoint()).thenReturn(Collections.emptyList());
        when(jibConfigurationService.getUser()).thenReturn(Optional.empty());
        when(jibConfigurationService.getOutputPathsTar()).thenReturn(Optional.empty());
        when(jibConfigurationService.getFromPlatforms()).thenReturn(Collections.emptySet());
        when(jibConfigurationService.resolveCredentialForImage(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any()))
            .thenReturn(Optional.empty());
        var applicationConfigurationService = mock(ApplicationConfigurationService.class);
        when(applicationConfigurationService.getServerPort()).thenReturn("8080");
        var mojo = new NativeImageJibMojo(project, jibConfigurationService, applicationConfigurationService, null,
            mockSession(project, userProperties, systemProperties), mojoExecution);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "dockerBuild";
        mojo.baseImageRun = AbstractDockerMojo.DEFAULT_BASE_IMAGE_GRAALVM_RUN;
        mojo.enabled = true;
        return mojo;
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getArtifactId()).thenReturn("demo");
        when(project.getVersion()).thenReturn("0.1");
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getBuild()).thenReturn(build);
        when(project.getPlugin("org.graalvm.buildtools:native-maven-plugin")).thenReturn(null);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        return project;
    }

    private static Plugin nativeImagePlugin(String imageNameValue) {
        var plugin = new Plugin();
        var configuration = new Xpp3Dom("configuration");
        var imageName = new Xpp3Dom("imageName");
        imageName.setValue(imageNameValue);
        configuration.addChild(imageName);
        plugin.setConfiguration(configuration);
        return plugin;
    }

    private static MojoExecution mockMojoExecution(Xpp3Dom configuration) {
        var mojoExecution = mock(MojoExecution.class);
        when(mojoExecution.getConfiguration()).thenReturn(configuration);
        return mojoExecution;
    }

    private static Xpp3Dom mojoConfiguration(String name, String value) {
        var configuration = new Xpp3Dom("configuration");
        var child = new Xpp3Dom(name);
        child.setValue(value);
        configuration.addChild(child);
        return configuration;
    }

    private static Xpp3Dom descriptorDefaultMojoConfiguration() {
        var configuration = new Xpp3Dom("configuration");
        var child = new Xpp3Dom("jibBuildGoal");
        child.setValue("${jib.buildGoal}");
        child.setAttribute("default-value", "dockerBuild");
        configuration.addChild(child);
        return configuration;
    }

    private static MavenSession mockSession(MavenProject project, Properties userProperties, Properties systemProperties) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(systemProperties);
        when(session.getUserProperties()).thenReturn(userProperties);
        return session;
    }
}
