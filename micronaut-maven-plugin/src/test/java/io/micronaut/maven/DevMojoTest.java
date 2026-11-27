package io.micronaut.maven;

import io.micronaut.maven.dev.BuildToolWatcher;
import io.micronaut.maven.dev.ControlPanel;
import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.classworlds.ClassWorld;
import org.codehaus.plexus.component.configurator.BasicComponentConfigurator;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.configuration.PlexusConfiguration;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.transfer.ArtifactNotFoundException;
import org.eclipse.aether.transfer.ArtifactTransferException;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.io.Reader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Stream;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class DevMojoTest {

    @TempDir
    Path directory;

    private MavenSession mavenSession;
    private MavenSession runnableSession;
    private CompilerService compilerService;
    private ExecutorService executorService;
    private RepositorySystem repositorySystem;
    private ComponentConfigurator configurator;
    private MojoExecution mojoExecution;
    private MavenProject app;
    private DevMojo mojo;

    @BeforeEach
    void setUp() throws Exception {
        mavenSession = mock(MavenSession.class);
        runnableSession = mock(MavenSession.class);
        when(mavenSession.clone()).thenReturn(runnableSession);
        when(mavenSession.getUserProperties()).thenReturn(new Properties());
        when(mavenSession.getRepositorySession()).thenReturn(mock(RepositorySystemSession.class));
        when(runnableSession.getSystemProperties()).thenReturn(new Properties());
        when(runnableSession.getUserProperties()).thenReturn(new Properties());
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        compilerService = mock(CompilerService.class);
        executorService = mock(ExecutorService.class);
        repositorySystem = mock(RepositorySystem.class);
        configurator = mock(ComponentConfigurator.class);
        mojoExecution = mock(MojoExecution.class);
        app = project(directory.resolve("app"), "app", true);
        when(runnableSession.getCurrentProject()).thenReturn(app);
        mojo = new DevMojo(mavenSession, toolchainManager, compilerService, executorService, repositorySystem, configurator);
        setField("mojoExecution", mojoExecution);
        setField("mainClass", "example.Application");
        setField("strategy", "auto");
        setField("compile", "embedded");
        setField("incremental", true);
        setField("liveReload", true);
        setField("liveReloadPort", 35729);
        setField("liveReloadInjectScript", true);
    }

    @Test
    void theManifestIsWrittenForTheRunnableProject() throws Exception {
        File runtimeJar = Files.createFile(directory.resolve("runtime.jar")).toFile();
        File compileJar = Files.createFile(directory.resolve("compile.jar")).toFile();
        when(mavenSession.getCurrentProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(dependency(runtimeJar), new Dependency(new DefaultArtifact("example:unresolved:1.0"), JavaScopes.RUNTIME)));
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE)).thenReturn(List.of(dependency(compileJar)));
        setField("manifestOnly", true);
        setField("retain", " example.Pool, ,example.Client ");

        mojo.execute();

        Path manifestDirectory = directory.resolve("app/target/micronaut-dev");
        Properties manifest = new Properties();
        try (Reader reader = Files.newBufferedReader(manifestDirectory.resolve(DevManifest.FILE_NAME))) {
            manifest.load(reader);
        }
        assertEquals("example.Application", manifest.getProperty("micronaut.dev.main-class"));
        assertEquals("example.Pool,example.Client", manifest.getProperty("micronaut.dev.retain"));
        assertEquals(List.of(runtimeJar.getAbsolutePath()), Files.readAllLines(manifestDirectory.resolve("runtime.argfile")));
        assertEquals(List.of(compileJar.getAbsolutePath()), Files.readAllLines(manifestDirectory.resolve("compile.argfile")));
        assertEquals(List.of(), Files.readAllLines(manifestDirectory.resolve("processors.argfile")));
        verifyNoInteractions(repositorySystem);
    }

    @Test
    void aMissingMainClassFailsTheGoal() throws Exception {
        when(mavenSession.getCurrentProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        setField("mainClass", null);

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);
        assertTrue(failure.getMessage().contains("No main class"));
    }

    @Test
    void anUnknownCompileModeFailsTheGoalBeforeAnythingStarts() throws Exception {
        when(mavenSession.getCurrentProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        setField("compile", "buildtool");

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Unknown value 'buildtool' for mn.dev.compile: the valid values are embedded, build-tool", failure.getMessage());
        verifyNoInteractions(compilerService);
        assertFalse(Files.exists(directory.resolve("app/target/micronaut-dev")));
    }

    @Test
    void anUnknownStrategyFailsTheGoalBeforeAnythingStarts() throws Exception {
        when(mavenSession.getCurrentProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        setField("strategy", "resart");

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Unknown value 'resart' for mn.dev.strategy: the valid values are restart, reload, auto", failure.getMessage());
        verifyNoInteractions(compilerService);
    }

    @Test
    void aSettingIsReadAsTheLauncherReadsIt() throws Exception {
        setField("compile", " BUILD_TOOL ");
        setField("strategy", "Reload");

        invoke("validateSettings");

        // the launcher reads the same mode from both spellings: without this, it waits for a trigger nothing touches
        assertTrue((boolean) invoke("isBuildToolMode"));
        assertEquals("build-tool", getField("compile"));
        assertEquals("reload", getField("strategy"));
    }

    @Test
    void theApplicationIsLaunchedAndItsFailureFailsTheGoal() throws Exception {
        MavenProject root = project(directory, "root", false);
        when(mavenSession.getCurrentProject()).thenReturn(root);
        when(mavenSession.getProjects()).thenReturn(List.of(root, app));
        when(mavenSession.getAllProjects()).thenReturn(List.of(root, app));
        reconfigurable();
        Files.createDirectories(directory.resolve("app/src/main/java"));
        app.addCompileSourceRoot(directory.resolve("app/src/main/java").toString());
        managed(app, "io.micronaut", "micronaut-dev", "5.3.0");
        managed(app, "io.micronaut", "micronaut-dev-livereload", "5.3.0");
        when(repositorySystem.resolveDependencies(any(), any())).thenAnswer(invocation -> resolve(invocation.getArgument(1)));
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(value("strategy", "restart"));
        app.getPlugin(THIS_PLUGIN).setConfiguration(configuration);
        app.getProperties().setProperty("exec.mainClass", "example.Application");
        Properties userProperties = new Properties();
        userProperties.setProperty("mn.dev.compile", "build-tool");
        when(runnableSession.getUserProperties()).thenReturn(userProperties);

        // the launcher jars are empty files: the JVM cannot start the launcher, and exits with an error
        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(failure.getMessage().contains("exited with status"), failure.getMessage());
        assertTrue(Files.isRegularFile(directory.resolve("app/target/micronaut-dev").resolve(DevManifest.FILE_NAME)));
        // the configuration of the selected application was applied
        assertEquals("restart", getField("strategy"));
        assertEquals("build-tool", getField("compile"));
    }

    @Test
    void theRunnableProjectIsTheOnlyOneDeclaringThePlugin() throws Exception {
        MavenProject root = project(directory, "root", false);
        MavenProject other = project(directory.resolve("other"), "other", true);
        when(mavenSession.getCurrentProject()).thenReturn(root);
        when(mavenSession.getProjects()).thenReturn(List.of(root, app));

        assertSame(app, invoke("findRunnableProject"));

        when(mavenSession.getProjects()).thenReturn(List.of(root, app, other));
        IllegalStateException failure = assertThrows(IllegalStateException.class, () -> invoke("findRunnableProject"));
        assertTrue(failure.getMessage().contains("[app, other]"));
    }

    @Test
    void theSelectedApplicationsConfigurationOfThisGoalIsApplied() throws Exception {
        Xpp3Dom pluginConfiguration = new Xpp3Dom("configuration");
        pluginConfiguration.addChild(value("strategy", "restart"));
        // a setting of mn:run only
        pluginConfiguration.addChild(value("watches", "src"));
        Xpp3Dom cliConfiguration = new Xpp3Dom("configuration");
        cliConfiguration.addChild(value("strategy", "reload"));
        cliConfiguration.addChild(value("livereloadPort", "4000"));
        Plugin plugin = app.getPlugin(THIS_PLUGIN);
        plugin.setConfiguration(pluginConfiguration);
        PluginExecution cli = new PluginExecution();
        cli.setId("default-cli");
        cli.setConfiguration(cliConfiguration);
        plugin.addExecution(cli);
        when(mojoExecution.getMojoDescriptor()).thenReturn(descriptor());
        app.getProperties().setProperty("exec.mainClass", "example.FromProperty");
        setField("runnableProject", app);

        invoke("applyRunnableProjectConfiguration");

        ArgumentCaptor<PlexusConfiguration> configuration = ArgumentCaptor.forClass(PlexusConfiguration.class);
        verify(configurator).configureComponent(eq(mojo), configuration.capture(), any(), any());
        PlexusConfiguration applied = configuration.getValue();
        assertEquals("reload", applied.getChild("strategy").getValue());
        assertEquals("4000", applied.getChild("livereloadPort").getValue(), "a parameter is matched by its alias too");
        assertNull(applied.getChild("watches", false));
        // the configuration names no main class: the goal's default, the application's exec.mainClass, applies
        assertEquals("${exec.mainClass}", applied.getChild("mainClass").getAttribute("default-value"));
    }

    @Test
    void anApplicationWithoutPluginConfigurationIsConfiguredWithTheGoalDefaults() throws Exception {
        when(mojoExecution.getMojoDescriptor()).thenReturn(descriptor());
        setField("runnableProject", app);

        invoke("applyRunnableProjectConfiguration");

        ArgumentCaptor<PlexusConfiguration> configuration = ArgumentCaptor.forClass(PlexusConfiguration.class);
        verify(configurator).configureComponent(eq(mojo), configuration.capture(), any(), any());
        // the defaults are evaluated against the selected application, so its exec.mainClass is the main class
        assertEquals("${exec.mainClass}", configuration.getValue().getChild("mainClass").getAttribute("default-value"));
        assertNull(configuration.getValue().getChild("strategy", false));
    }

    @Test
    void aSettingResolvedOnlyInTheReactorRootDoesNotRemain() throws Exception {
        reconfigurable();
        // as Maven configured the goal from the root, an aggregator the application does not inherit from
        setField("retain", "example.OnlyInTheRoot");
        setField("strategy", "restart");
        setField("mainClass", "example.FromTheRoot");
        app.getProperties().setProperty("exec.mainClass", "example.Application");
        setField("runnableProject", app);

        invoke("applyRunnableProjectConfiguration");

        assertNull(getField("retain"));
        assertEquals("auto", getField("strategy"), "the goal's default, which the root's value hid");
        assertEquals("example.Application", getField("mainClass"));
    }

    @Test
    void theApplicationsOwnValuesAndTheCommandLineApplyOverTheReset() throws Exception {
        reconfigurable();
        setField("retain", "example.OnlyInTheRoot");
        setField("incremental", false);
        app.getProperties().setProperty("mn.dev.retain", "example.FromTheApplication");
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(value("strategy", "restart"));
        app.getPlugin(THIS_PLUGIN).setConfiguration(configuration);
        Properties userProperties = new Properties();
        userProperties.setProperty("mn.dev.compile", "build-tool");
        when(runnableSession.getUserProperties()).thenReturn(userProperties);
        setField("runnableProject", app);

        invoke("applyRunnableProjectConfiguration");

        assertEquals("example.FromTheApplication", getField("retain"));
        assertEquals("restart", getField("strategy"));
        assertEquals("build-tool", getField("compile"));
        assertEquals(true, getField("incremental"), "the goal's default");
    }

    @Test
    void theExecutionSelectedOnTheCommandLineIsAppliedInsteadOfDefaultCli() throws Exception {
        reconfigurable();
        when(mojoExecution.getExecutionId()).thenReturn("local");
        Plugin plugin = app.getPlugin(THIS_PLUGIN);
        Xpp3Dom localConfiguration = new Xpp3Dom("configuration");
        localConfiguration.addChild(value("strategy", "restart"));
        PluginExecution local = new PluginExecution();
        local.setId("local");
        local.setConfiguration(localConfiguration);
        plugin.addExecution(local);
        Xpp3Dom cliConfiguration = new Xpp3Dom("configuration");
        cliConfiguration.addChild(value("strategy", "reload"));
        cliConfiguration.addChild(value("retain", "example.OnlyInDefaultCli"));
        PluginExecution cli = new PluginExecution();
        cli.setId("default-cli");
        cli.setConfiguration(cliConfiguration);
        plugin.addExecution(cli);
        setField("runnableProject", app);

        invoke("applyRunnableProjectConfiguration");

        assertEquals("restart", getField("strategy"));
        assertNull(getField("retain"));
    }

    @Test
    void theProcessorPathIsResolvedWithTheDependencyManagement() throws Exception {
        managed(app, "io.micronaut", "micronaut-inject-java", "5.0.0");
        setField("runnableProject", app);
        when(repositorySystem.resolveDependencies(any(), any())).thenAnswer(invocation -> resolve(invocation.getArgument(1)));
        List<Dependency> paths = List.of(
            new Dependency(new DefaultArtifact("io.micronaut", "micronaut-inject-java", "jar", ""), JavaScopes.RUNTIME),
            new Dependency(new DefaultArtifact("io.micronaut", "micronaut-unmanaged", "jar", ""), JavaScopes.RUNTIME),
            new Dependency(new DefaultArtifact("io.micronaut", "micronaut-validation-processor", "jar", "1.0.0"), JavaScopes.RUNTIME)
        );

        @SuppressWarnings("unchecked")
        List<File> files = (List<File>) invoke("resolveProcessorPath", new Class<?>[] {DevManifest.ProcessorPaths.class}, new DevManifest.ProcessorPaths(paths, true));

        assertEquals(List.of("micronaut-inject-java-5.0.0.jar", "micronaut-validation-processor-1.0.0.jar"), files.stream().map(File::getName).toList(),
            "a path without a managed version is left out");
        ArgumentCaptor<DependencyRequest> request = ArgumentCaptor.forClass(DependencyRequest.class);
        verify(repositorySystem).resolveDependencies(any(), request.capture());
        assertEquals(1, request.getValue().getCollectRequest().getManagedDependencies().size());
    }

    @Test
    void theLaunchArgumentsRunTheLauncherWithTheManifest() throws Exception {
        setField("runnableProject", app);
        File runtimeJar = directory.resolve("runtime.jar").toFile();
        File launcherJar = directory.resolve("micronaut-dev-5.3.0.jar").toFile();
        File liveReloadJar = directory.resolve("micronaut-dev-livereload-5.3.0.jar").toFile();
        DependencyResolutionService resolution = mock(DependencyResolutionService.class);
        when(resolution.artifactResultsFor(any(), eq(true))).thenReturn(List.of(
            result(new DefaultArtifact("io.micronaut", "micronaut-dev-livereload", "jar", "5.3.0"), liveReloadJar),
            result(new DefaultArtifact("io.micronaut", "micronaut-dev", "jar", "5.3.0"), launcherJar),
            result(runtimeJar)
        ));
        setField("dependencyResolutionService", resolution);
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(dependency(runtimeJar)));
        Properties userProperties = new Properties();
        userProperties.setProperty("micronaut.environments", "dev");
        when(mavenSession.getUserProperties()).thenReturn(userProperties);
        setField("debug", true);
        setField("debugSuspend", true);
        setField("debugHost", "localhost");
        setField("debugPort", 8000);
        setField("jvmArguments", "-Xmx512m \"-Dgreeting=hello world\"");
        setField("appArguments", "--port 8080");
        Path manifest = directory.resolve("dev.properties");

        @SuppressWarnings("unchecked")
        List<String> args = (List<String>) invoke("launchArguments", new Class<?>[] {Path.class}, manifest);
        // added on macOS only, for the native watch service
        args = new ArrayList<>(args);
        args.remove("--enable-native-access=ALL-UNNAMED");

        assertTrue(args.get(0).endsWith("java") || args.get(0).endsWith("java.exe"));
        assertEquals(List.of(
            "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=localhost:8000",
            "-Xmx512m",
            "-Dgreeting=hello world",
            "-Dmicronaut.environments=dev",
            "-javaagent:" + launcherJar.getAbsolutePath(),
            "-classpath",
            String.join(File.pathSeparator, runtimeJar.getAbsolutePath(), liveReloadJar.getAbsolutePath(), launcherJar.getAbsolutePath()),
            DevMojo.LAUNCHER_MAIN_CLASS,
            "--manifest",
            manifest.toString(),
            "--port",
            "8080"
        ), args.subList(1, args.size()));
    }

    @Test
    void anUnresolvedLauncherFailsTheGoal() throws Exception {
        setField("runnableProject", app);
        setField("liveReload", false);
        DependencyResolutionService resolution = mock(DependencyResolutionService.class);
        when(resolution.artifactResultsFor(any(), eq(true))).thenReturn(List.of());
        setField("dependencyResolutionService", resolution);

        Throwable failure = assertThrows(InvocationTargetException.class, () -> invoke("launchArguments", new Class<?>[] {Path.class}, directory)).getCause();

        assertInstanceOf(MojoExecutionException.class, failure);
        assertTrue(failure.getMessage().contains("micronaut-dev"));
    }

    @Test
    void aSuccessfulCompilationTouchesTheTrigger() throws Exception {
        setField("runnableProject", app);
        when(mavenSession.getTopLevelProject()).thenReturn(app);
        InvocationResult result = mock(InvocationResult.class);
        when(result.getExitCode()).thenReturn(0);
        when(executorService.invokeGoals(app, app, "compile")).thenReturn(result);
        Path trigger = directory.resolve("micronaut-dev/reload");

        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        assertTrue(Files.isRegularFile(trigger));
    }

    @Test
    void aCompilationSelectsTheRunnableProjectFromTheExecutionRoot() throws Exception {
        MavenProject root = project(directory.resolve("root"), "root", false);
        root.setExecutionRoot(true);
        setField("runnableProject", app);
        // the first of what -pl selected, when the execution root is not among them
        when(mavenSession.getTopLevelProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(root, app));
        InvocationResult result = mock(InvocationResult.class);
        when(result.getExitCode()).thenReturn(0);
        when(executorService.invokeGoals(root, app, "compile")).thenReturn(result);
        Path trigger = directory.resolve("micronaut-dev/reload");

        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        verify(executorService).invokeGoals(root, app, "compile");
        assertTrue(Files.isRegularFile(trigger));
    }

    @Test
    void aFailedCompilationDoesNotTouchTheTrigger() throws Exception {
        setField("runnableProject", app);
        when(mavenSession.getTopLevelProject()).thenReturn(app);
        InvocationResult result = mock(InvocationResult.class);
        when(result.getExitCode()).thenReturn(1);
        when(executorService.invokeGoals(app, app, "compile")).thenReturn(result);
        Path trigger = directory.resolve("micronaut-dev/reload");

        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        when(executorService.invokeGoals(app, app, "compile")).thenThrow(new IllegalStateException("no Maven"));
        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        assertFalse(Files.exists(trigger));
    }

    @Test
    void aCompilationAfterTheGoalFinishedDoesNothing() throws Exception {
        setField("finished", true);

        invoke("compileAndTouch", new Class<?>[] {Path.class}, directory.resolve("reload"));

        verify(executorService, never()).invokeGoals(any(MavenProject.class), any(MavenProject.class), any(String[].class));
        assertFalse(Files.exists(directory.resolve("reload")));
    }

    @Test
    void theWatcherWatchesTheExistingRootsOfTheRunnableProject() throws Exception {
        setField("runnableProject", app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app, project(directory.resolve("unrelated"), "unrelated", false)));

        assertNull(invoke("watchAndCompile", new Class<?>[] {Path.class}, directory.resolve("reload")), "nothing to watch");

        Files.createDirectories(directory.resolve("app/src/main/java"));
        app.addCompileSourceRoot(directory.resolve("app/src/main/java").toString());
        BuildToolWatcher watcher = (BuildToolWatcher) invoke("watchAndCompile", new Class<?>[] {Path.class}, directory.resolve("reload"));
        assertNotNull(watcher);
        watcher.close();
    }

    @Test
    void aLauncherThatSpentItsGenerationBudgetIsStartedAgain() throws Exception {
        Process spent = mock(Process.class);
        when(spent.waitFor()).thenReturn(3);
        Path marker = directory.resolve("relaunched");
        ProcessBuilder launch = new ProcessBuilder(javaExecutable(), "--version").redirectOutput(marker.toFile());

        int exit = (int) invoke("awaitExit", new Class<?>[] {Process.class, ProcessBuilder.class}, spent, launch);

        assertEquals(0, exit);
        assertTrue(Files.size(marker) > 0);
    }

    @Test
    void aLauncherThatExitsWhileTheGoalStopsIsNotStartedAgain() throws Exception {
        Process spent = mock(Process.class);
        when(spent.waitFor()).thenReturn(3);
        setField("stopping", true);
        Path marker = directory.resolve("relaunched");
        ProcessBuilder launch = new ProcessBuilder(javaExecutable(), "--version").redirectOutput(marker.toFile());

        int exit = (int) invoke("awaitExit", new Class<?>[] {Process.class, ProcessBuilder.class}, spent, launch);

        assertEquals(3, exit);
        assertFalse(Files.exists(marker));
    }

    @Test
    void anyOtherStatusOfTheLauncherIsReturned() throws Exception {
        Process failed = mock(Process.class);
        when(failed.waitFor()).thenReturn(1);
        Path marker = directory.resolve("relaunched");
        ProcessBuilder launch = new ProcessBuilder(javaExecutable(), "--version").redirectOutput(marker.toFile());

        int exit = (int) invoke("awaitExit", new Class<?>[] {Process.class, ProcessBuilder.class}, failed, launch);

        assertEquals(1, exit);
        assertFalse(Files.exists(marker));
    }

    @Test
    void theNativeWatchServiceIsAddedToTheLauncherOnMacOsOnly() throws Exception {
        String osName = System.getProperty("os.name");
        try {
            System.setProperty("os.name", "Mac OS X");
            assertTrue(launcherArtifactIds().contains("micronaut-runtime-osx"));
            System.setProperty("os.name", "Linux");
            assertFalse(launcherArtifactIds().contains("micronaut-runtime-osx"));
        } finally {
            System.setProperty("os.name", osName);
        }
    }

    private List<String> launcherArtifactIds() {
        return mojo.launcherArtifacts().stream().map(Artifact::getArtifactId).toList();
    }

    private static String javaExecutable() {
        return Path.of(System.getProperty("java.home"), "bin", "java").toString();
    }

    @Test
    void theApplicationIsStoppedWithTheGoal() throws Exception {
        Process process = mock(Process.class);
        when(process.isAlive()).thenReturn(true);
        @SuppressWarnings("unchecked")
        java.util.concurrent.atomic.AtomicReference<Process> reference = (java.util.concurrent.atomic.AtomicReference<Process>) getField("process");
        reference.set(process);

        invoke("killProcess");

        verify(process).destroy();
        verify(process).waitFor();
    }

    @Test
    void theGoalAddsTheModulesTheDependencyManagementVersionsToTheManifest() throws Exception {
        File runtimeJar = Files.createFile(directory.resolve("runtime.jar")).toFile();
        when(mavenSession.getCurrentProject()).thenReturn(app);
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(dependency(runtimeJar)));
        when(repositorySystem.resolveDependencies(any(), any())).thenAnswer(invocation -> resolve(invocation.getArgument(1)));
        managed(app, ControlPanel.PLATFORM_GROUP, "micronaut-parent", "5.2.4");
        managed(app, ControlPanel.GROUP, "micronaut-control-panel-ui", "2.2.0");
        managed(app, ControlPanel.GROUP, "micronaut-control-panel-management", "2.2.0");
        setField("controlPanel", true);
        setField("manifestOnly", true);
        Log log = logged();

        mojo.execute();

        Path manifestDirectory = directory.resolve("app/target/micronaut-dev");
        assertEquals(List.of(
            runtimeJar.getAbsolutePath(),
            directory.resolve("micronaut-control-panel-ui-2.2.0.jar").toFile().getAbsolutePath(),
            directory.resolve("micronaut-control-panel-management-2.2.0.jar").toFile().getAbsolutePath(),
            manifestDirectory.resolve(ControlPanel.RESOURCES_DIRECTORY_NAME).toFile().getAbsolutePath()
        ), Files.readAllLines(manifestDirectory.resolve("runtime.argfile")));
        verify(log).info(argThat((CharSequence line) -> line.toString().contains("io.micronaut.controlpanel:micronaut-control-panel-testing is not versioned by the dependency management")));
        verify(log).info(argThat((CharSequence line) -> line.toString().contains("[micronaut-control-panel-ui, micronaut-control-panel-management]")));
    }

    @Test
    void theControlPanelAddsWhatTheRuntimeClasspathDoesNotHoldOnce() throws Exception {
        File runtimeJar = directory.resolve("runtime.jar").toFile();
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(dependency(runtimeJar)));
        File shared = directory.resolve("shared.jar").toFile();
        List<String> requested = controlPanelOn("5.2.4", module -> switch (module) {
            // the application's own version of a module wins over the one a panel brings
            case "micronaut-control-panel-ui" -> List.of(panel(module), result(shared), result(directory.resolve("other/runtime.jar").toFile()));
            // what another panel brought is not added a second time, even from another file
            case "micronaut-control-panel-management" -> List.of(panel(module), result(directory.resolve("other/shared.jar").toFile()));
            default -> List.of(panel(module));
        });

        assertEquals(List.of(
            jar("micronaut-control-panel-ui"),
            shared,
            jar("micronaut-control-panel-management"),
            jar("micronaut-control-panel-testing"),
            controlPanelResources().toFile()
        ), controlPanelClasspath());
        assertEquals(ControlPanel.BASE_MODULES, requested);
        assertEquals(ControlPanel.DEFAULT_PROPERTIES, controlPanelSettings());
    }

    @Test
    void thePanelsOfTheModulesTheApplicationUsesAreAdded() throws Exception {
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(module("io.micronaut.kafka", "micronaut-kafka")));
        List<String> requested = controlPanelOn("5.2.4", module -> List.of(panel(module)));

        assertTrue(controlPanelClasspath().contains(jar("micronaut-control-panel-kafka")));
        assertEquals("micronaut-control-panel-kafka", requested.getLast());
    }

    @Test
    void theRuntimeAndTheLauncherShareOneResolutionOfTheControlPanel() throws Exception {
        File runtimeJar = Files.createFile(directory.resolve("runtime.jar")).toFile();
        File launcherJar = directory.resolve("micronaut-dev-5.3.0.jar").toFile();
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(dependency(runtimeJar)));
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        List<String> requested = controlPanelOn("5.2.4", module -> "micronaut-dev".equals(module)
            ? List.of(result(new DefaultArtifact("io.micronaut", "micronaut-dev", "jar", "5.3.0"), launcherJar))
            : List.of(panel(module)));
        Path manifestDirectory = directory.resolve("app/target/micronaut-dev");

        Path manifest = mojo.buildManifest(manifestDirectory).writeTo(manifestDirectory);
        @SuppressWarnings("unchecked")
        List<String> args = (List<String>) invoke("launchArguments", new Class<?>[] {Path.class}, manifest);

        List<String> runtime = new ArrayList<>(List.of(runtimeJar.getAbsolutePath()));
        controlPanelClasspath().forEach(file -> runtime.add(file.getAbsolutePath()));
        assertEquals(runtime, Files.readAllLines(manifestDirectory.resolve("runtime.argfile")));
        assertEquals(5, runtime.size());
        runtime.add(launcherJar.getAbsolutePath());
        assertEquals(String.join(File.pathSeparator, runtime), args.get(args.indexOf("-classpath") + 1),
            "the runtime classpath, then the Control Panel, then the launcher");
        assertEquals(ControlPanel.BASE_MODULES, requested.subList(0, 3));
        assertEquals(1, requested.stream().filter("micronaut-control-panel-ui"::equals).count());
    }

    @Test
    void aModuleTheApplicationDeclaresIsNotResolvedAgainAndGetsTheSettings() throws Exception {
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(ControlPanel.BASE_MODULES.stream()
            .map(module -> module(ControlPanel.GROUP, module))
            .toList());
        List<String> requested = controlPanelOn("5.2.4", module -> {
            throw new AssertionError("Resolved " + module);
        });
        Log log = logged();

        assertEquals(List.of(controlPanelResources().toFile()), controlPanelClasspath());
        assertEquals(List.of(), requested);
        assertEquals(ControlPanel.DEFAULT_PROPERTIES, controlPanelSettings());
        verify(log, never()).info(any(CharSequence.class));
    }

    @Test
    void theApplicationsSecurityKeepsDecidingWhoOpensThePanels() throws Exception {
        when(compilerService.resolveDependencies(app, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME)).thenReturn(List.of(module("io.micronaut.security", "micronaut-security")));
        controlPanelOn("5.2.4", module -> List.of(panel(module)));

        controlPanelClasspath();

        assertEquals(ControlPanel.SECURED_PROPERTIES, controlPanelSettings());
    }

    @Test
    void aModuleThatIsNotPublishedIsLeftOut() throws Exception {
        List<String> requested = controlPanelOn("5.2.4", module -> {
            if ("micronaut-control-panel-testing".equals(module)) {
                throw resolutionFailure(new ArtifactNotFoundException(new DefaultArtifact(ControlPanel.GROUP, module, "jar", "2.2.0"), null));
            }
            return List.of(panel(module));
        });
        Log log = logged();

        assertEquals(List.of(jar("micronaut-control-panel-ui"), jar("micronaut-control-panel-management"), controlPanelResources().toFile()), controlPanelClasspath());
        assertEquals(ControlPanel.BASE_MODULES, requested);
        verify(log).info(argThat((CharSequence line) -> line.toString().contains("io.micronaut.controlpanel:micronaut-control-panel-testing is not published")));
        verify(log).info(argThat((CharSequence line) -> line.toString().contains("[micronaut-control-panel-ui, micronaut-control-panel-management]")));
    }

    @Test
    void withoutAnyModuleThereIsNoControlPanelAndNoSettings() throws Exception {
        Path stale = staleControlPanelSettings();
        controlPanelOn("5.2.4", module -> {
            throw resolutionFailure(new ArtifactNotFoundException(new DefaultArtifact(ControlPanel.GROUP, module, "jar", "2.2.0"), null));
        });
        Log log = logged();

        assertEquals(List.of(), controlPanelClasspath());
        assertFalse(Files.exists(stale));
        verify(log, never()).info(argThat((CharSequence line) -> line.toString().startsWith("Adding Micronaut Control Panel")));
    }

    @Test
    void aModuleTheDependencyManagementDoesNotVersionIsLeftOutButNotWhatItBrought() throws Exception {
        File transitive = directory.resolve("transitive.jar").toFile();
        controlPanelOn("5.2.4", module -> "micronaut-control-panel-ui".equals(module)
            ? List.of(result(transitive), new ArtifactResult(new ArtifactRequest()))
            : List.of());
        Log log = logged();

        // none of the modules themselves resolved: there is no Control Panel to configure
        assertEquals(List.of(transitive), controlPanelClasspath());
        assertFalse(Files.exists(controlPanelResources()));
        verify(log).info(argThat((CharSequence line) -> line.toString().contains("io.micronaut.controlpanel:micronaut-control-panel-ui is not versioned by the dependency management")));
    }

    @Test
    void anyOtherResolutionFailureFailsTheGoalAndSaysHowToDisableIt() throws Exception {
        controlPanelOn("5.2.4", module -> {
            throw resolutionFailure(new ArtifactTransferException(new DefaultArtifact(ControlPanel.GROUP, module, "jar", "2.2.0"), null, "Connection refused"));
        });

        Throwable failure = assertThrows(InvocationTargetException.class, this::controlPanelClasspath).getCause();

        assertInstanceOf(MojoExecutionException.class, failure);
        assertTrue(failure.getMessage().contains("io.micronaut.controlpanel:micronaut-control-panel-ui"), failure.getMessage());
        assertTrue(failure.getMessage().contains("mn.dev.controlPanel=false"), failure.getMessage());
        assertInstanceOf(DependencyResolutionException.class, failure.getCause());
    }

    @Test
    void settingsThatCannotBeWrittenFailTheGoal() throws Exception {
        controlPanelOn("5.2.4", module -> List.of(panel(module)));
        // a file where the directory of the settings goes
        Files.createDirectories(controlPanelResources().getParent());
        Files.createFile(controlPanelResources());

        Throwable failure = assertThrows(InvocationTargetException.class, this::controlPanelClasspath).getCause();

        assertInstanceOf(MojoExecutionException.class, failure);
        assertTrue(failure.getMessage().startsWith("Cannot write the Micronaut Control Panel settings to " + controlPanelResources()), failure.getMessage());
    }

    @Test
    void anOlderPlatformLeavesTheControlPanelOutAndSaysWhy() throws Exception {
        Path stale = staleControlPanelSettings();
        List<String> requested = controlPanelOn("5.2.3", module -> List.of(panel(module)));
        Log log = logged();

        assertEquals(List.of(), controlPanelClasspath());
        assertEquals(List.of(), requested);
        assertFalse(Files.exists(stale));
        verify(log).warn(argThat((CharSequence line) -> line.toString().contains("requires Micronaut Platform 5.2.4 or later") && line.toString().contains("the build uses 5.2.3")));
    }

    @Test
    void aBuildWithoutThePlatformLeavesTheControlPanelOutAndSaysWhy() throws Exception {
        setField("runnableProject", app);
        setField("controlPanel", true);
        Log log = logged();

        assertEquals(List.of(), controlPanelClasspath());
        verify(log).warn(argThat((CharSequence line) -> line.toString().contains("the build uses no Micronaut Platform")));
    }

    @Test
    void aDisabledControlPanelResolvesNothingAndRemovesTheSettingsOfAnEarlierRun() throws Exception {
        Path stale = staleControlPanelSettings();
        List<String> requested = controlPanelOn("5.2.4", module -> List.of(panel(module)));
        setField("controlPanel", false);
        Log log = logged();

        assertEquals(List.of(), controlPanelClasspath());
        assertEquals(List.of(), requested);
        assertFalse(Files.exists(stale));
        verify(log, never()).warn(any(CharSequence.class));
    }

    @Test
    void theManifestSaysWhetherTheApplicationListensOnLocalhost() throws Exception {
        when(mavenSession.getAllProjects()).thenReturn(List.of(app));
        setField("runnableProject", app);
        Path manifestDirectory = directory.resolve("app/target/micronaut-dev");

        setField("bindLoopback", true);
        assertEquals("true", mojo.buildManifest(manifestDirectory).entries().get(DevManifest.BIND_LOOPBACK));
        setField("bindLoopback", false);
        assertEquals("false", mojo.buildManifest(manifestDirectory).entries().get(DevManifest.BIND_LOOPBACK));
    }

    /**
     * What a Control Panel module resolves to.
     */
    @FunctionalInterface
    private interface ModuleResolution {
        List<ArtifactResult> resolve(String artifactId) throws DependencyResolutionException;
    }

    /**
     * Turns the Control Panel on for a project of a platform.
     *
     * @return the artifact ids requested so far, each versioned by the dependency management
     */
    private List<String> controlPanelOn(String platform, ModuleResolution resolution) throws Exception {
        managed(app, ControlPanel.PLATFORM_GROUP, "micronaut-parent", platform);
        setField("runnableProject", app);
        setField("controlPanel", true);
        List<String> requested = new ArrayList<>();
        DependencyResolutionService service = mock(DependencyResolutionService.class);
        when(service.artifactResultsFor(any(), eq(true))).thenAnswer(invocation -> {
            Stream<Artifact> artifacts = invocation.getArgument(0);
            Artifact artifact = artifacts.findFirst().orElseThrow();
            assertEquals("", artifact.getVersion());
            requested.add(artifact.getArtifactId());
            return resolution.resolve(artifact.getArtifactId());
        });
        setField("dependencyResolutionService", service);
        return requested;
    }

    @SuppressWarnings("unchecked")
    private List<File> controlPanelClasspath() throws Exception {
        return (List<File>) invoke("controlPanelClasspath");
    }

    private Path controlPanelResources() {
        return directory.resolve("app/target/micronaut-dev").resolve(ControlPanel.RESOURCES_DIRECTORY_NAME);
    }

    private Map<String, String> controlPanelSettings() throws Exception {
        Properties settings = new Properties();
        try (Reader reader = Files.newBufferedReader(controlPanelResources().resolve(ControlPanel.PROPERTIES_RESOURCE))) {
            settings.load(reader);
        }
        Map<String, String> result = new HashMap<>();
        settings.stringPropertyNames().forEach(name -> result.put(name, settings.getProperty(name)));
        return result;
    }

    private Path staleControlPanelSettings() throws Exception {
        ControlPanel.writeResources(controlPanelResources(), List.of());
        Path settings = controlPanelResources().resolve(ControlPanel.PROPERTIES_RESOURCE);
        assertTrue(Files.isRegularFile(settings));
        return settings;
    }

    private Log logged() {
        Log log = mock(Log.class);
        mojo.setLog(log);
        return log;
    }

    private File jar(String module) {
        return directory.resolve(module + ".jar").toFile();
    }

    private ArtifactResult panel(String module) {
        return result(new DefaultArtifact(ControlPanel.GROUP, module, "jar", "2.2.0"), jar(module));
    }

    private Dependency module(String groupId, String artifactId) {
        return new Dependency(new DefaultArtifact(groupId, artifactId, "jar", "1.0").setFile(directory.resolve(artifactId + ".jar").toFile()), JavaScopes.RUNTIME);
    }

    private static DependencyResolutionException resolutionFailure(Exception cause) {
        ArtifactResult failed = new ArtifactResult(new ArtifactRequest(new DefaultArtifact(ControlPanel.GROUP, "failed", "jar", "2.2.0"), List.of(), null));
        failed.addException(cause);
        return new DependencyResolutionException(new DependencyResult(new DependencyRequest()), new ArtifactResolutionException(List.of(failed)));
    }

    private static MavenProject project(Path basedir, String artifactId, boolean withPlugin) throws Exception {
        Files.createDirectories(basedir);
        MavenProject project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId(artifactId);
        project.setVersion("1.0");
        project.setFile(basedir.resolve("pom.xml").toFile());
        project.getBuild().setDirectory(basedir.resolve("target").toString());
        project.getBuild().setOutputDirectory(basedir.resolve("target/classes").toString());
        if (withPlugin) {
            Plugin plugin = new Plugin();
            plugin.setGroupId("io.micronaut.maven");
            plugin.setArtifactId("micronaut-maven-plugin");
            project.getBuild().addPlugin(plugin);
        }
        return project;
    }

    private static void managed(MavenProject project, String groupId, String artifactId, String version) {
        if (project.getDependencyManagement() == null) {
            project.getModel().setDependencyManagement(new DependencyManagement());
        }
        org.apache.maven.model.Dependency dependency = new org.apache.maven.model.Dependency();
        dependency.setGroupId(groupId);
        dependency.setArtifactId(artifactId);
        dependency.setVersion(version);
        project.getDependencyManagement().addDependency(dependency);
    }

    private static MojoDescriptor descriptor() throws Exception {
        MojoDescriptor descriptor = new MojoDescriptor();
        descriptor.setPluginDescriptor(new PluginDescriptor());
        descriptor.addParameter(parameter("strategy", null));
        descriptor.addParameter(parameter("liveReloadPort", "livereloadPort"));
        descriptor.addParameter(parameter("mainClass", null));
        // the goal's defaults, as plugin.xml declares them
        XmlPlexusConfiguration defaults = new XmlPlexusConfiguration("configuration");
        XmlPlexusConfiguration mainClass = new XmlPlexusConfiguration("mainClass");
        mainClass.setAttribute("default-value", "${exec.mainClass}");
        defaults.addChild(mainClass);
        descriptor.setMojoConfiguration(defaults);
        return descriptor;
    }

    /**
     * The goal with Maven's configurator and the expressions and defaults its plugin.xml declares.
     */
    private void reconfigurable() throws Exception {
        mojo = new DevMojo(mavenSession, mock(ToolchainManager.class), compilerService, executorService, repositorySystem, new BasicComponentConfigurator());
        setField("mojoExecution", mojoExecution);
        when(mojoExecution.getMojoDescriptor()).thenReturn(pluginDescriptorOfTheGoal());
    }

    private static MojoDescriptor pluginDescriptorOfTheGoal() throws Exception {
        MojoDescriptor descriptor = new MojoDescriptor();
        PluginDescriptor plugin = new PluginDescriptor();
        plugin.setClassRealm(new ClassWorld("test", DevMojoTest.class.getClassLoader()).getClassRealm("test"));
        descriptor.setPluginDescriptor(plugin);
        XmlPlexusConfiguration configuration = new XmlPlexusConfiguration("configuration");
        for (String[] parameter : List.of(
            new String[] {"mainClass", String.class.getName(), null, "${exec.mainClass}"},
            new String[] {"strategy", String.class.getName(), "${mn.dev.strategy}", "auto"},
            new String[] {"compile", String.class.getName(), "${mn.dev.compile}", "embedded"},
            new String[] {"incremental", "boolean", "${mn.dev.incremental}", "true"},
            new String[] {"retain", String.class.getName(), "${mn.dev.retain}", null})) {
            org.apache.maven.plugin.descriptor.Parameter declared = parameter(parameter[0], null);
            declared.setType(parameter[1]);
            declared.setEditable(true);
            descriptor.addParameter(declared);
            XmlPlexusConfiguration child = new XmlPlexusConfiguration(parameter[0]);
            child.setValue(parameter[2]);
            if (parameter[3] != null) {
                child.setAttribute("default-value", parameter[3]);
            }
            configuration.addChild(child);
        }
        descriptor.setMojoConfiguration(configuration);
        return descriptor;
    }

    private static org.apache.maven.plugin.descriptor.Parameter parameter(String name, String alias) {
        org.apache.maven.plugin.descriptor.Parameter parameter = new org.apache.maven.plugin.descriptor.Parameter();
        parameter.setName(name);
        parameter.setAlias(alias);
        return parameter;
    }

    /**
     * Resolves every dependency of the request, with a version, to a file named after it.
     */
    private DependencyResult resolve(DependencyRequest request) {
        List<ArtifactResult> results = new ArrayList<>();
        for (Dependency dependency : request.getCollectRequest().getDependencies()) {
            Artifact artifact = dependency.getArtifact();
            results.add(result(artifact, directory.resolve(artifact.getArtifactId() + "-" + artifact.getVersion() + ".jar").toFile()));
        }
        DependencyResult result = new DependencyResult(request);
        result.setArtifactResults(results);
        return result;
    }

    private static ArtifactResult result(File file) {
        return result(new DefaultArtifact("example", file.getName(), "jar", "1.0"), file);
    }

    /**
     * The result of resolving an artifact, with its coordinates: the launcher is found among the results by them.
     */
    private static ArtifactResult result(Artifact artifact, File file) {
        ArtifactResult result = new ArtifactResult(new ArtifactRequest());
        result.setArtifact(artifact.setFile(file));
        return result;
    }

    private static Dependency dependency(File file) {
        return new Dependency(new DefaultArtifact("example", file.getName(), "jar", "1.0").setFile(file), JavaScopes.RUNTIME);
    }

    private static Xpp3Dom value(String name, String value) {
        Xpp3Dom dom = new Xpp3Dom(name);
        dom.setValue(value);
        return dom;
    }

    private Object invoke(String name) throws Exception {
        return invoke(name, new Class<?>[0]);
    }

    private Object invoke(String name, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = DevMojo.class.getDeclaredMethod(name, parameterTypes);
        method.setAccessible(true);
        try {
            return method.invoke(mojo, args);
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof RuntimeException runtime) {
                throw runtime;
            }
            throw e;
        }
    }

    private void setField(String name, Object value) throws Exception {
        field(name).set(mojo, value);
    }

    private Object getField(String name) throws Exception {
        return field(name).get(mojo);
    }

    private static Field field(String name) throws NoSuchFieldException {
        for (Class<?> type = DevMojo.class; type != null; type = type.getSuperclass()) {
            try {
                Field field = type.getDeclaredField(name);
                field.setAccessible(true);
                return field;
            } catch (NoSuchFieldException _) {
                // declared higher up
            }
        }
        throw new NoSuchFieldException(name);
    }
}
