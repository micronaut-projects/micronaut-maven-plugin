package io.micronaut.maven;

import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.command.RootFS;
import com.github.dockerjava.api.exception.ConflictException;
import com.github.dockerjava.api.exception.NotFoundException;
import com.github.dockerjava.api.model.ContainerConfig;
import com.github.dockerjava.api.model.ExposedPort;
import io.micronaut.maven.jdkaotcache.MicronautJars;
import io.micronaut.maven.jib.JibConfiguration;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
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
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atLeastOnce;
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
        // Only Jib's packaged mode, which the JDK AOT cache uses, needs the application JAR
        verify(executorService, never()).executeGoal(project, "org.apache.maven.plugins:maven-jar-plugin", "jar");
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
        var images = new DaemonImages();
        var dockerService = trainingDockerService(images);
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
        // Jib's packaged mode puts the application JAR in the image
        order.verify(executorService).executeGoal(project, "org.apache.maven.plugins:maven-jar-plugin", "jar");
        order.verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "dockerBuild");
        order.verify(dockerService).createContainer(eq("sha256:training"), isNull(), anyMap());
        order.verify(dockerService).removeContainer("container");
        order.verify(executorService).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "buildTar");
        order.verify(dockerService).removeImage("demo-jdk-aot-training");
        verify(dockerService, never()).removeImage("sha256:training");
        assertFalse(images.exists("sha256:training"));
        assertEquals(List.of("registry.example.com/demo:1.0"), images.tagsOf("sha256:final"));

        var training = buildProperties.get(0);
        assertEquals("packaged", training.getProperty("jib.containerizingMode"));
        assertEquals("linux/arm64", training.getProperty(DockerMojo.JDK_AOT_CACHE_PLATFORM_PROPERTY));
        assertEquals("demo-jdk-aot-training", training.getProperty("jib.to.image"));
        assertNull(training.getProperty(DockerMojo.JDK_AOT_CACHE_FILE_PROPERTY));

        var cacheFile = tempDir.resolve("target/jdk-aot-cache/app.aot");
        var image = buildProperties.get(1);
        assertEquals("packaged", image.getProperty("jib.containerizingMode"));
        assertEquals("linux/arm64", image.getProperty(DockerMojo.JDK_AOT_CACHE_PLATFORM_PROPERTY));
        assertNull(image.getProperty("jib.to.image"));
        assertEquals(cacheFile.toAbsolutePath().toString(), image.getProperty(DockerMojo.JDK_AOT_CACHE_FILE_PROPERTY));
        assertEquals("true", image.getProperty(DockerMojo.JDK_AOT_CACHE_PIN_BASE_IMAGE_PROPERTY));
        assertEquals("cache", Files.readString(cacheFile));

        for (String property : List.of("jib.containerizingMode", "jib.to.image", DockerMojo.JDK_AOT_CACHE_PLATFORM_PROPERTY,
            DockerMojo.JDK_AOT_CACHE_FILE_PROPERTY, DockerMojo.JDK_AOT_CACHE_PIN_BASE_IMAGE_PROPERTY)) {
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
        verify(dockerService).removeImage("demo-jdk-aot-training");
    }

    @Test
    void jdkAotCacheKeepsTheOtherTagsOfTheTrainingImage(@TempDir Path tempDir) throws Exception {
        // Jib images are reproducible: a packaged image that the project built before has the ID of the training image
        var project = mockProject(tempDir);
        var images = new DaemonImages();
        var dockerService = trainingDockerService(images);
        images.tag("demo:packaged", "sha256:training");
        images.tag("demo:other", "sha256:training");

        jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, mock(ExecutorService.class)).execute();

        verify(dockerService).removeImage("demo-jdk-aot-training");
        verify(dockerService, never()).removeImage("sha256:training");
        assertEquals(List.of("demo:packaged", "demo:other"), images.tagsOf("sha256:training"));
    }

    @Test
    void jdkAotCacheKeepsTheImageWhenJibToImageIsAUserPropertyAndRemovesTheUntaggedTrainingImage(@TempDir Path tempDir)
        throws Exception {
        // The user property has precedence over the tag of the training image, so Jib tags the training image with the
        // name of the final image, and the final build moves the tag
        var project = mockProject(tempDir);
        var images = new DaemonImages();
        var dockerService = trainingDockerService(images);
        images.untag("demo-jdk-aot-training");
        images.untag("registry.example.com/demo:1.0");
        var userProperties = new Properties();
        userProperties.setProperty("jib.to.image", "demo:1.0");
        var executorService = mock(ExecutorService.class);
        var builtImages = new ArrayList<>(List.of("sha256:training", "sha256:final"));
        doAnswer(invocation -> {
            images.tag("demo:1.0", builtImages.remove(0));
            return null;
        }).when(executorService).executeGoal(eq(project), eq("com.google.cloud.tools:jib-maven-plugin"), any());
        var mojo = new DockerMojo(project, jdkAotCacheJibConfiguration(), null, dockerService,
            mockSession(project, userProperties), mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "dockerBuild";
        setParameter(mojo, "jdkAotCache", true);
        setParameter(mojo, "jdkAotCacheTrainingPaths", List.of("/hello"));
        setParameter(mojo, "jdkAotCacheTrainingTimeout", 180);

        mojo.execute();

        verify(dockerService, never()).removeImage("demo:1.0");
        verify(dockerService).removeImage("sha256:training");
        assertEquals(List.of("demo:1.0"), images.tagsOf("sha256:final"));
        assertFalse(images.exists("sha256:training"));
    }

    @Test
    void jdkAotCacheDoesNotPinDaemonBaseImages(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var jibConfigurationService = jdkAotCacheJibConfiguration();
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("docker://eclipse-temurin:25-jre"));
        var executorService = mock(ExecutorService.class);
        var pinned = new ArrayList<String>();
        doAnswer(invocation -> {
            pinned.add(project.getProperties().getProperty(DockerMojo.JDK_AOT_CACHE_PIN_BASE_IMAGE_PROPERTY));
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
        verify(dockerService).removeImage("demo-jdk-aot-training");
        assertFalse(project.getProperties().containsKey(DockerMojo.JDK_AOT_CACHE_PLATFORM_PROPERTY));
    }

    @Test
    void jdkAotCacheTrainsWithoutStartingTheApplicationWhenTheMicronautVersionCan(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withLoadMode(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var dockerService = trainingDockerService();
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);
        setParameter(mojo, "jdkAotCacheTrainingPaths", null);
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertEquals("-XX:AOTCacheOutput=/tmp/app.aot -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=load", trainingEnvironment(dockerService).get("JDK_JAVA_OPTIONS"));
        verify(dockerService).startAndWait("container", "sha256:training", 180);
        verify(dockerService, never()).execInContainer(any(), anyInt(), any(), any(String[].class));
        verify(dockerService, never()).signalContainer(any(), any());
        verify(executorService, times(2)).executeGoal(project, "com.google.cloud.tools:jib-maven-plugin", "dockerBuild");
        assertTrue(infoMessages(log).contains("JDK AOT cache: training mode load, the default: the application loads its bean definitions and "
            + "exits without starting, so the training needs none of the services that its beans use. If the "
            + "application can start in the image build, micronaut.docker.jdkAotCache.trainingMode=start trains a more "
            + "complete cache"));
        assertEquals(List.of(), warnMessages(log));
        assertEquals("cache", Files.readString(tempDir.resolve("target/jdk-aot-cache/app.aot")));
    }

    @Test
    void jdkAotCacheStartsTheApplicationWhenStartIsConfigured(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withLoadMode(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var dockerService = trainingDockerService();
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, mock(ExecutorService.class));
        setParameter(mojo, "jdkAotCacheTrainingMode", "start");
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertEquals("-XX:AOTCacheOutput=/tmp/app.aot -XX:-UsePerfData -Dmicronaut.application.training.enabled=true "
            + "-Dmicronaut.application.training.mode=start -Dmicronaut.application.training.warmup.paths[0]=/hello",
            trainingEnvironment(dockerService).get("JDK_JAVA_OPTIONS"));
        verify(dockerService).startAndWait("container", "sha256:training", 180);
        assertTrue(infoMessages(log).contains("JDK AOT cache: training mode start: the training run starts the application"));
        // The output of the application has the warnings of Micronaut about the requests answered with 400 to 499
        verify(dockerService).logContainerOutput("container");
        assertEquals(List.of(), warnMessages(log));
    }

    @Test
    void jdkAotCacheStartsTheApplicationAndSaysWhyWhenTheMicronautVersionHasNoTrainingMode(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withoutSwitch(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var dockerService = trainingDockerService();
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, mock(ExecutorService.class));
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertEquals("-XX:AOTCacheOutput=/tmp/app.aot -XX:-UsePerfData", trainingEnvironment(dockerService).get("JDK_JAVA_OPTIONS"));
        verify(dockerService).signalContainer("container", "SIGTERM");
        assertTrue(infoMessages(log).contains("JDK AOT cache: training mode start: the training run starts the application, because its "
            + "Micronaut version has no training mode that loads it without starting it "
            + "(micronaut.application.training.mode). The services it needs at start-up must be reachable from the image "
            + "build. With a Micronaut version that has that mode, load becomes the default: set "
            + "micronaut.docker.jdkAotCache.trainingMode=start to keep starting the application"));
        // Training paths without a mode: the build that fails once the Micronaut version has the load mode
        assertEquals(List.of("JDK AOT cache: micronaut.docker.jdkAotCache.trainingPaths is set and "
            + "micronaut.docker.jdkAotCache.trainingMode is not. This build will fail once the application uses a "
            + "Micronaut version with the load training mode: load becomes the default, and a load training run does "
            + "not start the application, so it sends no requests. Set micronaut.docker.jdkAotCache.trainingMode=start "
            + "now to keep starting the application"), warnMessages(log));
    }

    @Test
    void jdkAotCacheDoesNotWarnWithoutTrainingPathsWhenTheMicronautVersionHasNoTrainingMode(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withoutSwitch(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), trainingDockerService(), mock(ExecutorService.class));
        setParameter(mojo, "jdkAotCacheTrainingPaths", null);
        var log = mock(Log.class);
        mojo.setLog(log);

        mojo.execute();

        assertTrue(infoMessages(log).stream().anyMatch(message -> message.startsWith("JDK AOT cache: training mode start: the "
            + "training run starts the application, because its Micronaut version has no training mode")));
        assertEquals(List.of(), warnMessages(log));
    }

    @Test
    void jdkAotCacheRejectsTrainingPathsWithTheDefaultLoadMode(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withLoadMode(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var dockerService = mock(DockerService.class);
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().startsWith("micronaut.docker.jdkAotCache.trainingPaths is set, but the training mode is "
            + "load, the default with this Micronaut version"), ex.getMessage());
        verifyNoInteractions(executorService, dockerService);
    }

    @Test
    void jdkAotCacheRejectsTheLoadModeOnAMicronautVersionWithoutIt(@TempDir Path tempDir) throws Exception {
        var project = mockProject(tempDir);
        var artifacts = new LinkedHashSet<>(MicronautJars.withSwitch(tempDir));
        when(project.getArtifacts()).thenReturn(artifacts);
        var dockerService = mock(DockerService.class);
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);
        setParameter(mojo, "jdkAotCacheTrainingPaths", List.of());
        setParameter(mojo, "jdkAotCacheTrainingMode", "load");

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(ex.getMessage().startsWith("micronaut.docker.jdkAotCache.trainingMode=load needs a Micronaut version "
            + "with the load training mode"), ex.getMessage());
        verifyNoInteractions(executorService, dockerService);
    }

    @Test
    void jdkAotCacheRejectsAnUnknownTrainingMode(@TempDir Path tempDir) {
        var project = mockProject(tempDir);
        var dockerService = mock(DockerService.class);
        var executorService = mock(ExecutorService.class);
        var mojo = jdkAotCacheMojo(project, jdkAotCacheJibConfiguration(), dockerService, executorService);
        setParameter(mojo, "jdkAotCacheTrainingMode", "warm-up");

        var ex = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Invalid micronaut.docker.jdkAotCache.trainingMode 'warm-up': it must be load or start", ex.getMessage());
        verifyNoInteractions(executorService, dockerService);
    }

    /**
     * @return the INFO messages of the mojo, without the colour that its log adds
     */
    private static List<String> infoMessages(Log log) {
        ArgumentCaptor<CharSequence> messages = ArgumentCaptor.forClass(CharSequence.class);
        verify(log, atLeastOnce()).info(messages.capture());
        return messages.getAllValues().stream()
            .map(message -> message.toString().replaceAll("\u001B\\[[;\\d]*m", ""))
            .toList();
    }

    private static List<String> warnMessages(Log log) {
        ArgumentCaptor<CharSequence> messages = ArgumentCaptor.forClass(CharSequence.class);
        verify(log, atLeast(0)).warn(messages.capture());
        return messages.getAllValues().stream()
            .map(message -> message.toString().replaceAll("\u001B\\[[;\\d]*m", ""))
            .toList();
    }

    private static Map<String, String> trainingEnvironment(DockerService dockerService) {
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, String>> environment = ArgumentCaptor.forClass(Map.class);
        verify(dockerService).createContainer(eq("sha256:training"), isNull(), environment.capture());
        return environment.getValue();
    }

    private static DockerMojo jdkAotCacheMojo(MavenProject project, JibConfigurationService jibConfigurationService,
                                              DockerService dockerService, ExecutorService executorService) {
        var mojo = new DockerMojo(project, jibConfigurationService, null, dockerService, mockSession(project),
            mock(MojoExecution.class), executorService);
        mojo.micronautRuntime = "NONE";
        mojo.jibBuildGoal = "dockerBuild";
        setParameter(mojo, "jdkAotCache", true);
        setParameter(mojo, "jdkAotCacheTrainingPaths", List.of("/hello"));
        setParameter(mojo, "jdkAotCacheTrainingTimeout", 180);
        return mojo;
    }

    private static JibConfigurationService jdkAotCacheJibConfiguration() {
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
        when(jibConfigurationService.getToImage()).thenReturn(Optional.of("registry.example.com/demo:1.0"));
        return jibConfigurationService;
    }

    private static DockerService trainingDockerService() throws IOException {
        return trainingDockerService(new DaemonImages());
    }

    /**
     * @param images the images of the daemon, which get the training image as {@code demo-jdk-aot-training} and the
     * final image as {@code registry.example.com/demo:1.0}
     */
    private static DockerService trainingDockerService(DaemonImages images) throws IOException {
        var dockerService = mock(DockerService.class);
        when(dockerService.getDaemonPlatform()).thenReturn("linux/arm64");
        images.stub(dockerService,
            inspectResponse("sha256:training", "sha256:base", "sha256:app"),
            inspectResponse("sha256:final", "sha256:base", "sha256:app", "sha256:cache"));
        images.tag("demo-jdk-aot-training", "sha256:training");
        images.tag("registry.example.com/demo:1.0", "sha256:final");
        when(dockerService.runAndCaptureOutput(eq("sha256:training"), anyInt(), any()))
            .thenReturn(new DockerService.ContainerOutput(0, "openjdk version \"25.0.4\" 2026-07-21 LTS"));
        when(dockerService.createContainer(eq("sha256:training"), isNull(), anyMap())).thenReturn("container");
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
        return mockSession(project, new Properties());
    }

    private static MavenSession mockSession(MavenProject project, Properties userProperties) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(userProperties);
        return session;
    }

    /**
     * The images of a Docker daemon, as tags that refer to image IDs. As with {@code docker rmi} without force, removing
     * the last tag of an image deletes the image, and removing an image by its ID fails when several tags refer to it.
     */
    private static final class DaemonImages {

        private final Map<String, InspectImageResponse> images = new HashMap<>();
        private final Map<String, String> tags = new LinkedHashMap<>();

        void stub(DockerService dockerService, InspectImageResponse... responses) {
            for (InspectImageResponse image : responses) {
                images.put(image.getId(), image);
                when(image.getRepoTags()).thenAnswer(invocation -> tagsOf(image.getId()));
            }
            when(dockerService.inspectImage(any())).thenAnswer(invocation -> {
                String name = invocation.getArgument(0);
                InspectImageResponse image = images.get(tags.getOrDefault(name, name));
                if (image == null) {
                    throw new NotFoundException("No such image: " + name);
                }
                return image;
            });
            doAnswer(invocation -> {
                remove(invocation.getArgument(0));
                return null;
            }).when(dockerService).removeImage(any());
        }

        void tag(String tag, String imageId) {
            tags.put(tag, imageId);
        }

        void untag(String tag) {
            tags.remove(tag);
        }

        List<String> tagsOf(String imageId) {
            return tags.entrySet().stream()
                .filter(tag -> tag.getValue().equals(imageId))
                .map(Map.Entry::getKey)
                .toList();
        }

        boolean exists(String imageId) {
            return images.containsKey(imageId);
        }

        private void remove(String name) {
            String imageId = tags.remove(name);
            if (imageId == null) {
                List<String> imageTags = tagsOf(name);
                if (imageTags.size() > 1) {
                    throw new ConflictException("unable to delete " + name + " (must be forced) - image is referenced "
                        + "in multiple repositories");
                }
                imageTags.forEach(tags::remove);
                images.remove(name);
            } else if (tagsOf(imageId).isEmpty()) {
                images.remove(imageId);
            }
        }
    }

    /**
     * Sets a parameter of the mojo, which Maven injects into a private field.
     */
    private static void setParameter(DockerMojo mojo, String name, Object value) {
        try {
            Field field = DockerMojo.class.getDeclaredField(name);
            field.setAccessible(true);
            field.set(mojo, value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }
}
