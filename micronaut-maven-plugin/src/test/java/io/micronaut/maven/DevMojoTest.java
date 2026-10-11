package io.micronaut.maven;

import io.micronaut.maven.dev.BuildToolWatcher;
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
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.configuration.PlexusConfiguration;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResult;
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
import java.util.List;
import java.util.Properties;

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
    void theApplicationIsLaunchedAndItsFailureFailsTheGoal() throws Exception {
        MavenProject root = project(directory, "root", false);
        when(mavenSession.getCurrentProject()).thenReturn(root);
        when(mavenSession.getProjects()).thenReturn(List.of(root, app));
        when(mavenSession.getAllProjects()).thenReturn(List.of(root, app));
        when(mojoExecution.getMojoDescriptor()).thenReturn(descriptor());
        Files.createDirectories(directory.resolve("app/src/main/java"));
        app.addCompileSourceRoot(directory.resolve("app/src/main/java").toString());
        managed(app, "io.micronaut", "micronaut-dev", "5.3.0");
        managed(app, "io.micronaut", "micronaut-dev-livereload", "5.3.0");
        when(repositorySystem.resolveDependencies(any(), any())).thenAnswer(invocation -> resolve(invocation.getArgument(1)));
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(value("strategy", "restart"));
        app.getPlugin(THIS_PLUGIN).setConfiguration(configuration);
        app.getProperties().setProperty("exec.mainClass", "example.Application");
        setField("compile", "build-tool");

        // the launcher jars are empty files: the JVM cannot start the launcher, and exits with an error
        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertTrue(failure.getMessage().contains("exited with status"), failure.getMessage());
        assertTrue(Files.isRegularFile(directory.resolve("app/target/micronaut-dev").resolve(DevManifest.FILE_NAME)));
        // the configuration of the selected application was applied
        verify(configurator).configureComponent(eq(mojo), any(PlexusConfiguration.class), any(), any());
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
        when(mavenSession.getTopLevelProject()).thenReturn(app);
        InvocationResult result = mock(InvocationResult.class);
        when(result.getExitCode()).thenReturn(0);
        when(executorService.invokeGoals(app, "compile")).thenReturn(result);
        Path trigger = directory.resolve("micronaut-dev/reload");

        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        assertTrue(Files.isRegularFile(trigger));
    }

    @Test
    void aFailedCompilationDoesNotTouchTheTrigger() throws Exception {
        when(mavenSession.getTopLevelProject()).thenReturn(app);
        InvocationResult result = mock(InvocationResult.class);
        when(result.getExitCode()).thenReturn(1);
        when(executorService.invokeGoals(app, "compile")).thenReturn(result);
        Path trigger = directory.resolve("micronaut-dev/reload");

        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        when(executorService.invokeGoals(app, "compile")).thenThrow(new IllegalStateException("no Maven"));
        invoke("compileAndTouch", new Class<?>[] {Path.class}, trigger);

        assertFalse(Files.exists(trigger));
    }

    @Test
    void aCompilationAfterTheGoalFinishedDoesNothing() throws Exception {
        setField("finished", true);

        invoke("compileAndTouch", new Class<?>[] {Path.class}, directory.resolve("reload"));

        verify(executorService, never()).invokeGoals(any(MavenProject.class), any(String[].class));
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
            } catch (NoSuchFieldException e) {
                // declared higher up
            }
        }
        throw new NoSuchFieldException(name);
    }
}
