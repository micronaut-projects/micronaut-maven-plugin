package io.micronaut.maven;

import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.command.RootFS;
import com.github.dockerjava.api.model.ContainerConfig;
import com.github.dockerjava.api.model.ExposedPort;
import io.micronaut.maven.jib.JdkAotCachePlan;
import io.micronaut.maven.jib.JibConfiguration;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class DockerMojoTest {

    @Test
    void executesConfiguredJibGoalWithinCurrentBuild(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "buildTar";

        mojo.execute();

        verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildTar");
    }

    @Test
    void rejectsJibRegistryBuildWithoutTargetImage(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "build";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("jib.buildGoal=build requires a configured target image"));
        assertTrue(ex.getMessage().contains("jib.to.image"));
        verify(executorService, never()).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "build");
    }

    @Test
    void executesJibRegistryBuildWithTargetImage(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getToImage()).thenReturn(Optional.of("registry.example.com/team/app:1.0"));
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "build";

        mojo.execute();

        verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "build");
    }

    @Test
    void rejectsUnknownJibGoal(@TempDir Path tempDir) throws MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        var executorService = mock(ExecutorService.class);
        var session = mockSession(project);

        var mojo = new DockerMojo(project, jibConfigurationService, null, null, session,
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "buildDirectory";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals(
            "Unsupported jib.buildGoal 'buildDirectory'. Supported values are: dockerBuild, build, buildTar",
            ex.getMessage()
        );
        verify(executorService, never()).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildDirectory");
    }

    @Test
    void jdkAotCacheOnlySupportsTheDefaultRuntime(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, mock(JibConfigurationService.class), mock(DockerService.class), executorService);
        mojo.micronautRuntime = "lambda";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("micronaut.docker.jdkAotCache only supports the default runtime, not lambda", ex.getMessage());
        verifyNoInteractions(executorService);
    }

    @Test
    void jdkAotCacheNeedsJibDefaultEntrypoint(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("eclipse-temurin:25-jre"));
        when(jibConfigurationService.getEntrypoint()).thenReturn(List.of("/start.sh"));
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jibConfigurationService, mock(DockerService.class), executorService);

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("Remove the Jib container.entrypoint configuration"));
        verifyNoInteractions(executorService);
    }

    @Test
    void jdkAotCacheNeedsADockerDaemonAlsoForBuildTar(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("eclipse-temurin:25-jre"));
        var dockerService = mock(DockerService.class);
        when(dockerService.getDaemonPlatform()).thenThrow(new IllegalStateException("Cannot connect to the Docker daemon at unix:///var/run/docker.sock. Is the docker daemon running?"));
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jibConfigurationService, dockerService, executorService);
        mojo.jibBuildGoal = "buildTar";

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("micronaut.docker.jdkAotCache runs the application in a Docker container to train the cache, so it "
            + "needs a Docker daemon, also with jib.buildGoal=buildTar or build. Cannot connect to the Docker daemon at "
            + "unix:///var/run/docker.sock. Is the docker daemon running?", ex.getMessage());
        verifyNoInteractions(executorService);
    }

    @Test
    void jdkAotCacheRejectsPlatformsOtherThanTheDaemonOne(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("eclipse-temurin:25-jre"));
        when(jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.of("amd64"), Optional.of("linux")),
            new JibConfiguration.PlatformConfiguration(Optional.of("aarch64"), Optional.of("linux"))));
        var dockerService = mock(DockerService.class);
        when(dockerService.getDaemonPlatform()).thenReturn("linux/arm64");
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jibConfigurationService, dockerService, executorService);

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("whose platform is linux/arm64"));
        assertTrue(ex.getMessage().contains("remove it, or set it to linux/arm64 only"));
        verifyNoInteractions(executorService);
    }

    @Test
    void jdkAotCacheAcceptsTheDaemonPlatform(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var jibConfigurationService = jdkAotCacheJibConfiguration();
        when(jibConfigurationService.getFromPlatforms()).thenReturn(Set.of(
            new JibConfiguration.PlatformConfiguration(Optional.of("aarch64"), Optional.of("linux"))));
        var dockerService = trainingDockerService();
        var executorService = mock(ExecutorService.class);

        jdkAotCacheMojo(project, jibConfigurationService, dockerService, executorService).execute();

        verify(executorService, times(2)).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "dockerBuild");
    }

    @Test
    void jdkAotCacheBuildsATrainingImageTrainsAndRebuildsWithTheConfiguredGoal(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var dockerService = trainingDockerService();
        var executorService = mock(ExecutorService.class);
        var buildProperties = new ArrayList<Properties>();
        doAnswer(invocation -> {
            var snapshot = new Properties();
            snapshot.putAll(project.getProperties());
            buildProperties.add(snapshot);
            return null;
        }).when(executorService).executeGoal(eq(project), eq("com.google.cloud.tools:jib-maven-plugin"), any());
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);
        mojo.jibBuildGoal = "buildTar";

        mojo.execute();

        var order = inOrder(executorService, dockerService);
        order.verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "dockerBuild");
        order.verify(dockerService).createContainer(eq("sha256:training"), isNull(), eq(false), anyMap());
        order.verify(dockerService).removeContainer("container");
        order.verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildTar");
        order.verify(dockerService).removeImage("sha256:training");

        var training = buildProperties.get(0);
        assertEquals("packaged", training.getProperty("jib.containerizingMode"));
        assertEquals("linux/arm64", training.getProperty(JdkAotCachePlan.PLATFORM_PROPERTY));
        assertEquals("demo-jdk-aot-training", training.getProperty("jib.to.image"));
        assertNull(training.getProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY));

        var cacheFile = tempDir.resolve("target/jdk-aot-cache/app.aot");
        var image = buildProperties.get(1);
        assertEquals("packaged", image.getProperty("jib.containerizingMode"));
        assertEquals("linux/arm64", image.getProperty(JdkAotCachePlan.PLATFORM_PROPERTY));
        assertNull(image.getProperty("jib.to.image"));
        assertEquals(cacheFile.toAbsolutePath().toString(), image.getProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY));
        assertEquals("true", image.getProperty(JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY));
        assertEquals("cache", Files.readString(cacheFile));

        for (String property : List.of("jib.containerizingMode", "jib.to.image", JdkAotCachePlan.PLATFORM_PROPERTY,
            JdkAotCachePlan.CACHE_FILE_PROPERTY, JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY)) {
            assertFalse(project.getProperties().containsKey(property), property);
        }
    }

    @Test
    void jdkAotCacheComparesTheLayersOfTheFinalDockerImage(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var dockerService = trainingDockerService();
        var finalImage = inspectResponse("sha256:final", "sha256:base", "sha256:changed", "sha256:cache");
        when(dockerService.inspectImage("registry.example.com/demo:1.0")).thenReturn(finalImage);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, mock(ExecutorService.class));

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().contains("differ from the layers of the training image"));
        verify(dockerService).removeImage("sha256:training");
    }

    @Test
    void jdkAotCacheDoesNotPinDaemonBaseImages(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var jibConfigurationService = jdkAotCacheJibConfiguration();
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("docker://eclipse-temurin:25-jre"));
        var executorService = mock(ExecutorService.class);
        var pinned = new ArrayList<String>();
        doAnswer(invocation -> {
            pinned.add(project.getProperties().getProperty(JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY));
            return null;
        }).when(executorService).executeGoal(eq(project), eq("com.google.cloud.tools:jib-maven-plugin"), any());

        jdkAotCacheMojo(project, jibConfigurationService, trainingDockerService(), executorService).execute();

        assertEquals(Arrays.asList(null, "false"), pinned);
    }

    @Test
    void jdkAotCacheRemovesTheTrainingImageWhenTheTrainingFails(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var dockerService = trainingDockerService();
        when(dockerService.execInContainer(eq("container"), anyInt(), any(), any(String[].class))).thenReturn(1);
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);

        assertThrows(MojoExecutionException.class, mojo::execute);

        verify(executorService, times(1)).executeGoal(eq(project), eq("com.google.cloud.tools:jib-maven-plugin"), any());
        verify(dockerService).removeContainer("container");
        verify(dockerService).removeImage("sha256:training");
        assertFalse(project.getProperties().containsKey(JdkAotCachePlan.PLATFORM_PROPERTY));
    }

    private static DockerMojo jdkAotCacheMojo(MavenProject project, JibConfigurationService jibConfigurationService,
                                              DockerService dockerService, ExecutorService executorService) {
        var mojo = new DockerMojo(project, jibConfigurationService, null, dockerService, mockSession(project),
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "dockerBuild";
        mojo.jdkAotCache = true;
        mojo.jdkAotCacheTrainingPaths = List.of("/hello");
        mojo.jdkAotCacheTrainingTimeout = 180;
        return mojo;
    }

    private static JibConfigurationService jdkAotCacheJibConfiguration() {
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        when(jibConfigurationService.getToImage()).thenReturn(Optional.of("registry.example.com/demo:1.0"));
        return jibConfigurationService;
    }

    private static DockerService trainingDockerService() throws IOException {
        var dockerService = mock(DockerService.class);
        when(dockerService.getDaemonPlatform()).thenReturn("linux/arm64");
        var trainingImage = inspectResponse("sha256:training", "sha256:base", "sha256:app");
        when(dockerService.inspectImage("demo-jdk-aot-training")).thenReturn(trainingImage);
        when(dockerService.inspectImage("sha256:training")).thenReturn(trainingImage);
        var finalImage = inspectResponse("sha256:final", "sha256:base", "sha256:app", "sha256:cache");
        when(dockerService.inspectImage("registry.example.com/demo:1.0")).thenReturn(finalImage);
        when(dockerService.runAndCaptureOutput(eq("sha256:training"), anyInt(), any()))
            .thenReturn(new DockerService.ContainerOutput(0, "openjdk version \"25.0.4\" 2026-07-21 LTS"));
        when(dockerService.createContainer(eq("sha256:training"), isNull(), eq(false), anyMap())).thenReturn("container");
        when(dockerService.awaitExit("container", 180)).thenReturn(143);
        doAnswer(invocation -> {
            Files.writeString(invocation.<Path>getArgument(2), "cache");
            return null;
        }).when(dockerService).copyFileFromContainer(eq("container"), eq("/tmp/app.aot"), any());
        return dockerService;
    }

    private static InspectImageResponse inspectResponse(String id, String... layers) {
        var config = mock(ContainerConfig.class);
        when(config.getExposedPorts()).thenReturn(new ExposedPort[] {ExposedPort.tcp(8080)});
        var image = mock(InspectImageResponse.class);
        when(image.getId()).thenReturn(id);
        when(image.getConfig()).thenReturn(config);
        when(image.getRootFS()).thenReturn(new RootFS().withLayers(List.of(layers)));
        return image;
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        when(project.getArtifactId()).thenReturn("demo");
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getBuild()).thenReturn(build);
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        return project;
    }

    private static MavenSession mockSession(MavenProject project) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(new Properties());
        return session;
    }
}
