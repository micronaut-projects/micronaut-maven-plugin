/*
 * Copyright 2017-2026 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven;

import io.methvin.watcher.DirectoryChangeEvent;
import io.methvin.watcher.DirectoryWatcher;
import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import io.micronaut.maven.testresources.AbstractTestResourcesMojo;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.classworlds.realm.ClassRealm;
import org.codehaus.plexus.component.configurator.ComponentConfigurationException;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.cli.CommandLineUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.util.filter.DependencyFilterUtils;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static io.micronaut.maven.core.MojoUtils.findJavaExecutable;
import static io.micronaut.maven.core.MojoUtils.hasMicronautMavenPlugin;

/**
 * Runs the application in development mode through {@code io.micronaut.dev.MicronautDevMain}, the
 * launcher of Micronaut Core's {@code micronaut-dev} module: sources are compiled inside the running
 * JVM and the application is restarted on a fresh class loader, with the singletons a retention
 * policy names kept alive, or, for an edit that only changed method bodies, the classes are
 * redefined in place. {@code mn:run} is unchanged; this goal is the alternative to it.
 *
 * <p>The goal writes the manifest the launcher reads into {@code target/micronaut-dev} from the
 * project model, the compiler plugin's configuration included, then launches the JVM with the
 * runtime classpath plus the launcher. With {@code mn.dev.compile=build-tool} the launcher compiles
 * nothing: this goal watches the sources, compiles through Maven as {@code mn:run} does, and touches
 * the trigger file the launcher watches.</p>
 *
 * @since 5.1.0
 */
@SuppressWarnings("unused")
@Mojo(name = DevMojo.MOJO_NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE, aggregator = true)
@Execute(phase = LifecyclePhase.PROCESS_CLASSES)
public class DevMojo extends AbstractTestResourcesMojo {

    /**
     * The name of the goal.
     */
    public static final String MOJO_NAME = "dev";

    /**
     * The launcher's main class.
     */
    public static final String LAUNCHER_MAIN_CLASS = "io.micronaut.dev.MicronautDevMain";

    private static final String LAUNCHER_GROUP = "io.micronaut";
    private static final String LAUNCHER_ARTIFACT = "micronaut-dev";
    private static final String LIVERELOAD_ARTIFACT = "micronaut-dev-livereload";
    private static final long QUIET_PERIOD_MILLIS = 300;
    private static final String DEFAULT_CLI_EXECUTION = "default-cli";

    private final MavenSession mavenSession;
    private final ToolchainManager toolchainManager;
    private final CompilerService compilerService;
    private final ExecutorService executorService;
    private final RepositorySystem repositorySystem;
    private final ComponentConfigurator configurator;
    private final String javaExecutable;
    /**
     * Resolves for the runnable project, which an aggregator invocation selects among the reactor: the
     * service bound to the current project would read the reactor root's management and repositories.
     */
    private DependencyResolutionService dependencyResolutionService;

    /**
     * The application's main class.
     */
    @Parameter(defaultValue = RunMojo.EXEC_MAIN_CLASS)
    private String mainClass;

    /**
     * The reload strategy: {@code restart}, {@code reload} or {@code auto}.
     */
    @Parameter(property = "mn.dev.strategy", defaultValue = "auto")
    private String strategy;

    /**
     * How sources are compiled after a change: {@code embedded}, in the development JVM, or
     * {@code build-tool}, by Maven.
     */
    @Parameter(property = "mn.dev.compile", defaultValue = "embedded")
    private String compile;

    /**
     * Whether the embedded compilers compile incrementally.
     */
    @Parameter(property = "mn.dev.incremental", defaultValue = "true")
    private boolean incremental;

    /**
     * The types whose singletons are kept across a restart, comma separated.
     */
    @Parameter(property = "mn.dev.retain")
    private String retain;

    /**
     * Whether the LiveReload module is added to the launcher's classpath.
     */
    @Parameter(property = "mn.dev.livereload", defaultValue = "true")
    private boolean liveReload;

    /**
     * The port the LiveReload server listens on.
     */
    @Parameter(property = "mn.dev.livereload.port", defaultValue = "35729")
    private int liveReloadPort;

    /**
     * Whether the LiveReload client script is appended to HTML responses; false for users of a browser extension.
     */
    @Parameter(property = "mn.dev.livereload.injectScript", defaultValue = "true")
    private boolean liveReloadInjectScript;

    /**
     * Writes the manifest and stops, without launching anything.
     */
    @Parameter(property = "mn.dev.manifestOnly", defaultValue = "false")
    private boolean manifestOnly;

    /**
     * Whether to start the application in debug mode (suspended until a debugger connects when {@code mn.debug.suspend} is set).
     */
    @Parameter(property = "mn.debug", defaultValue = "false")
    private boolean debug;

    /**
     * Whether to suspend the application until a debugger connects.
     */
    @Parameter(property = "mn.debug.suspend", defaultValue = "false")
    private boolean debugSuspend;

    /**
     * The debug port.
     */
    @Parameter(property = "mn.debug.port", defaultValue = "5005")
    private int debugPort;

    /**
     * The debug host.
     */
    @Parameter(property = "mn.debug.host", defaultValue = "127.0.0.1")
    private String debugHost;

    /**
     * JVM arguments for the application.
     */
    @Parameter(property = "mn.jvmArgs")
    private String jvmArguments;

    /**
     * Arguments for the application's {@code main}.
     */
    @Parameter(property = RunMojo.MN_APP_ARGS)
    private String appArguments;

    @Parameter(defaultValue = "${mojoExecution}", readonly = true, required = true)
    private MojoExecution mojoExecution;

    private MavenProject runnableProject;
    private volatile Process process;
    /**
     * Set when the build is shutting down, Ctrl+C for one: the application is stopped, and the status it
     * exits with is not a failure.
     */
    private volatile boolean stopping;

    /**
     * Constructor.
     *
     * @param mavenSession the session
     * @param toolchainManager the toolchain manager
     * @param compilerService the compiler service
     * @param executorService the executor service
     * @param repositorySystem the repository system
     * @param configurator Maven's configurator of mojos, which applies the selected application's configuration
     */
    @Inject
    public DevMojo(MavenSession mavenSession,
                   ToolchainManager toolchainManager,
                   CompilerService compilerService,
                   ExecutorService executorService,
                   RepositorySystem repositorySystem,
                   @Named("basic") ComponentConfigurator configurator) {
        this.mavenSession = mavenSession;
        this.toolchainManager = toolchainManager;
        this.compilerService = compilerService;
        this.executorService = executorService;
        this.repositorySystem = repositorySystem;
        this.configurator = configurator;
        this.javaExecutable = findJavaExecutable(toolchainManager, mavenSession);
    }

    @Override
    public void execute() throws MojoExecutionException {
        runnableProject = findRunnableProject();
        dependencyResolutionService = new DependencyResolutionService(mavenSession, runnableProject, repositorySystem);
        if (!runnableProject.equals(mavenSession.getCurrentProject())) {
            applyRunnableProjectConfiguration();
        }
        Path manifestDirectory = Path.of(runnableProject.getBuild().getDirectory()).resolve(DevManifest.DIRECTORY_NAME);
        Path manifestFile;
        try {
            manifestFile = buildManifest().writeTo(manifestDirectory);
        } catch (IOException e) {
            throw new MojoExecutionException("Cannot write the development mode manifest", e);
        }
        getLog().info("Development mode manifest written to " + manifestFile);
        if (manifestOnly) {
            return;
        }
        TestResourcesHelperAccess testResources = new TestResourcesHelperAccess();
        try {
            testResources.start();
            List<String> args = launchArguments(manifestFile);
            if (getLog().isDebugEnabled()) {
                getLog().debug("Running " + String.join(" ", args));
            }
            process = new ProcessBuilder(args).inheritIO().directory(runnableProject.getBasedir()).start();
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                stopping = true;
                killProcess();
            }));
            if (isBuildToolMode()) {
                watchAndCompile(manifestDirectory.resolve(DevManifest.TRIGGER_FILE_NAME));
            }
            int exit = process.waitFor();
            if (exit != 0 && !stopping) {
                // the application failed to start, or stopped on an error: the build says so
                throw new MojoExecutionException("The application exited with status " + exit);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (IOException | DependencyResolutionException e) {
            throw new MojoExecutionException("Cannot run the application in development mode: " + e.getMessage(), e);
        } finally {
            killProcess();
            testResources.stop();
        }
    }

    /**
     * An evaluator of the runnable project's configuration: its expressions resolve against it, not against
     * the reactor root an aggregator invocation started from.
     */
    private PluginParameterExpressionEvaluator runnableEvaluator() {
        MavenSession runnableSession = mavenSession.clone();
        runnableSession.setCurrentProject(runnableProject);
        return new PluginParameterExpressionEvaluator(runnableSession, mojoExecution);
    }

    /**
     * The goal's parameters as the selected application declares them. Maven configured this mojo from the
     * project the goal was invoked on, the reactor root when {@code mn:dev} runs from an aggregator, so the
     * application's effective configuration is applied here with Maven's own configurator: its plugin-level
     * {@code micronaut-maven-plugin} configuration with the {@code default-cli} execution's merged over it,
     * as Maven would have applied them had the goal run on the application, restricted to this goal's
     * parameters. The main class is evaluated against the application when its configuration names none.
     */
    private void applyRunnableProjectConfiguration() throws MojoExecutionException {
        PluginParameterExpressionEvaluator evaluator = runnableEvaluator();
        Xpp3Dom effective = effectiveConfiguration();
        try {
            if (effective != null) {
                MojoDescriptor descriptor = mojoExecution.getMojoDescriptor();
                Xpp3Dom own = new Xpp3Dom("configuration");
                for (Xpp3Dom child : effective.getChildren()) {
                    // another goal's settings, such as mn:run's watches, are not this goal's to apply
                    if (isParameter(descriptor, child.getName())) {
                        own.addChild(new Xpp3Dom(child));
                    }
                }
                ClassRealm realm = descriptor.getPluginDescriptor().getClassRealm();
                configurator.configureComponent(this, new XmlPlexusConfiguration(own), evaluator, realm);
                if (own.getChild("mainClass") != null) {
                    return;
                }
            }
            Object evaluated = evaluator.evaluate(RunMojo.EXEC_MAIN_CLASS);
            mainClass = evaluated == null ? null : evaluated.toString();
        } catch (ComponentConfigurationException | ExpressionEvaluationException e) {
            throw new MojoExecutionException("Cannot apply the micronaut-maven-plugin configuration of " + runnableProject.getArtifactId() + ": " + e.getMessage(), e);
        }
    }

    private Xpp3Dom effectiveConfiguration() {
        org.apache.maven.model.Plugin plugin = runnableProject.getPlugin(MojoUtils.THIS_PLUGIN);
        if (plugin == null) {
            return null;
        }
        Xpp3Dom configuration = plugin.getConfiguration() instanceof Xpp3Dom dom ? new Xpp3Dom(dom) : null;
        for (PluginExecution execution : plugin.getExecutions()) {
            if (DEFAULT_CLI_EXECUTION.equals(execution.getId()) && execution.getConfiguration() instanceof Xpp3Dom executionDom) {
                configuration = configuration == null ? new Xpp3Dom(executionDom) : Xpp3Dom.mergeXpp3Dom(new Xpp3Dom(executionDom), configuration);
            }
        }
        return configuration;
    }

    private static boolean isParameter(MojoDescriptor descriptor, String name) {
        if (descriptor.getParameterMap().containsKey(name)) {
            return true;
        }
        return descriptor.getParameters() != null && descriptor.getParameters().stream().anyMatch(parameter -> name.equals(parameter.getAlias()));
    }

    /**
     * Resolves the processor path as Maven does: each path with the exclusions it declares, a path without a
     * version versioned by the runnable project's dependency management.
     */
    private List<File> resolveProcessorPath(DevManifest.ProcessorPaths paths) throws DependencyResolutionException {
        CollectRequest request = new CollectRequest();
        request.setRepositories(runnableProject.getRemoteProjectRepositories());
        Map<String, String> managedVersions = new HashMap<>();
        if (runnableProject.getDependencyManagement() != null) {
            for (org.apache.maven.model.Dependency managed : runnableProject.getDependencyManagement().getDependencies()) {
                managedVersions.putIfAbsent(managed.getGroupId() + ":" + managed.getArtifactId(), managed.getVersion());
            }
            if (paths.managed()) {
                request.setManagedDependencies(runnableProject.getDependencyManagement().getDependencies().stream()
                    .map(DependencyResolutionService::mavenDependencyToAetherDependency)
                    .toList());
            }
        }
        for (Dependency dependency : paths.dependencies()) {
            org.eclipse.aether.artifact.Artifact artifact = dependency.getArtifact();
            if (artifact.getVersion() == null || artifact.getVersion().isEmpty()) {
                String version = managedVersions.get(artifact.getGroupId() + ":" + artifact.getArtifactId());
                if (version == null) {
                    getLog().warn("No version for the processor path entry " + artifact.getGroupId() + ":" + artifact.getArtifactId() + ": it is left out");
                    continue;
                }
                dependency = dependency.setArtifact(artifact.setVersion(version));
            }
            request.addDependency(dependency);
        }
        DependencyRequest dependencyRequest = new DependencyRequest(request, DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME));
        DependencyResult result = repositorySystem.resolveDependencies(mavenSession.getRepositorySession(), dependencyRequest);
        return DependencyResolutionService.toClasspathFiles(result.getArtifactResults());
    }

    private MavenProject findRunnableProject() {
        MavenProject currentProject = mavenSession.getCurrentProject();
        if (hasMicronautMavenPlugin(currentProject)) {
            return currentProject;
        }
        List<MavenProject> projectsWithPlugin = mavenSession.getProjects().stream()
            .filter(MojoUtils::hasMicronautMavenPlugin)
            .toList();
        if (projectsWithPlugin.size() == 1) {
            getLog().info("Running project %s".formatted(projectsWithPlugin.get(0).getArtifactId()));
            return projectsWithPlugin.get(0);
        }
        throw new IllegalStateException("The Micronaut Maven Plugin is declared in the following projects: %s. Please specify the project to run with the -pl option."
            .formatted(projectsWithPlugin.stream().map(MavenProject::getArtifactId).toList()));
    }

    private DevManifest buildManifest() throws MojoExecutionException {
        PluginParameterExpressionEvaluator evaluator = runnableEvaluator();
        List<File> runtimeClasspath = files(compilerService.resolveDependencies(runnableProject, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME));
        List<File> compileClasspath = files(compilerService.resolveDependencies(runnableProject, true, JavaScopes.PROVIDED, JavaScopes.COMPILE));
        List<File> processorPath;
        try {
            DevManifest.ProcessorPaths paths = DevManifest.processorPaths(runnableProject, evaluator);
            processorPath = paths.dependencies().isEmpty() ? List.of() : resolveProcessorPath(paths);
        } catch (Exception e) {
            throw new MojoExecutionException("Cannot resolve the annotation processor path: " + e.getMessage(), e);
        }
        List<MavenProject> reactor = mavenSession.getAllProjects().stream().filter(this::isDependencyOfRunnableProject).toList();
        String main = mainClass != null ? mainClass : runnableProject.getProperties().getProperty("exec.mainClass");
        if (main == null) {
            throw new MojoExecutionException("No main class: set exec.mainClass or the mainClass parameter");
        }
        List<String> retained = retain == null || retain.isBlank() ? List.of() : Arrays.stream(retain.split(",")).map(String::trim).filter(s -> !s.isEmpty()).toList();
        DevManifest.Settings settings = new DevManifest.Settings(main, strategy, compile, incremental, retained, liveReloadPort, liveReloadInjectScript);
        return DevManifest.of(runnableProject, reactor, settings, runtimeClasspath, compileClasspath, processorPath, evaluator);
    }

    private List<String> launchArguments(Path manifestFile) throws MojoExecutionException, DependencyResolutionException {
        List<File> launcher = DependencyResolutionService.toClasspathFiles(dependencyResolutionService.artifactResultsFor(launcherArtifacts(), true));
        if (launcher.isEmpty()) {
            throw new MojoExecutionException("Cannot resolve " + LAUNCHER_GROUP + ":" + LAUNCHER_ARTIFACT + ": development mode needs Micronaut Core 5.3 or later in the dependency management");
        }
        List<File> runtime = files(compilerService.resolveDependencies(runnableProject, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME));
        List<String> classpath = new ArrayList<>();
        for (File file : runtime) {
            classpath.add(file.getAbsolutePath());
        }
        for (File file : launcher) {
            if (!classpath.contains(file.getAbsolutePath())) {
                classpath.add(file.getAbsolutePath());
            }
        }
        List<String> translatedJvmArguments = translateArguments(jvmArguments);
        List<String> args = new ArrayList<>();
        args.add(javaExecutable);
        if (debug) {
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + (debugSuspend ? "y" : "n") + ",address=" + debugHost + ":" + debugPort);
        }
        addTestResourcesArguments(args);
        args.addAll(translatedJvmArguments);
        mavenSession.getUserProperties().forEach((key, value) -> args.add("-D" + key + "=" + value));
        launcher.stream()
            .filter(file -> file.getName().startsWith(LAUNCHER_ARTIFACT + "-") && !file.getName().startsWith(LIVERELOAD_ARTIFACT + "-"))
            .findFirst()
            // the launcher as the JVM's agent, for the method-body fast path
            .ifPresent(file -> args.add("-javaagent:" + file.getAbsolutePath()));
        args.add("-classpath");
        args.add(String.join(File.pathSeparator, classpath));
        args.add(LAUNCHER_MAIN_CLASS);
        args.add("--manifest");
        args.add(manifestFile.toString());
        args.addAll(translateArguments(appArguments));
        return args;
    }

    private Stream<org.eclipse.aether.artifact.Artifact> launcherArtifacts() {
        List<org.eclipse.aether.artifact.Artifact> artifacts = new ArrayList<>();
        // versioned by the dependency management, the platform BOM the parent imports
        artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, LAUNCHER_ARTIFACT, "jar", ""));
        if (liveReload) {
            artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, LIVERELOAD_ARTIFACT, "jar", ""));
        }
        return artifacts.stream();
    }

    private void addTestResourcesArguments(List<String> args) {
        if (testResourcesEnabled) {
            Path settingsDirectory = shared
                ? ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace)
                : AbstractTestResourcesMojo.serverSettingsDirectoryOf(Path.of(runnableProject.getBuild().getDirectory()));
            Optional<ServerSettings> serverSettings = ServerUtils.readServerSettings(settingsDirectory);
            serverSettings.ifPresent(settings -> new TestResourcesHelperAccess().helper().computeSystemProperties(settings)
                .forEach((key, value) -> args.add("-D" + key + "=" + value)));
        }
    }

    private boolean isBuildToolMode() {
        return "build-tool".equals(compile);
    }

    /**
     * In build-tool mode: the sources of the reactor are watched, a change compiles the project through
     * Maven, as {@code mn:run} does, and the trigger the launcher watches is touched once the compilation
     * finished, so that it applies the new classes.
     */
    private void watchAndCompile(Path trigger) throws IOException {
        List<Path> paths = new ArrayList<>();
        for (MavenProject project : mavenSession.getAllProjects()) {
            if (!isDependencyOfRunnableProject(project)) {
                continue;
            }
            // the roots as configured, a build-helper or a custom resource directory included
            for (String root : DevManifest.sourceRoots(project)) {
                Path directory = Path.of(root);
                if (Files.isDirectory(directory)) {
                    paths.add(directory);
                }
            }
        }
        if (paths.isEmpty()) {
            return;
        }
        ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(runnable -> {
            Thread thread = new Thread(runnable, "micronaut-dev-maven-compile");
            thread.setDaemon(true);
            return thread;
        });
        ScheduledFuture<?>[] pending = new ScheduledFuture<?>[1];
        DirectoryWatcher watcher = DirectoryWatcher.builder()
            .paths(paths)
            .listener((DirectoryChangeEvent event) -> {
                synchronized (pending) {
                    if (pending[0] != null) {
                        pending[0].cancel(false);
                    }
                    // a burst of events from one save becomes one compilation
                    pending[0] = scheduler.schedule(() -> compileAndTouch(trigger), QUIET_PERIOD_MILLIS, TimeUnit.MILLISECONDS);
                }
            })
            .build();
        getLog().info("👀 Watching for changes in " + paths.stream().map(path -> runnableProject.getBasedir().toPath().relativize(path).toString()).toList());
        Thread thread = new Thread(watcher::watch, "micronaut-dev-maven-watcher");
        thread.setDaemon(true);
        thread.start();
    }

    private void compileAndTouch(Path trigger) {
        // the invocation's exit code, not merely its return: a compiler error is a normal result of the invoker
        boolean compiled;
        try {
            MavenProject projectToCompile = mavenSession.getTopLevelProject();
            InvocationResult result = executorService.invokeGoals(projectToCompile, "compile");
            compiled = result.getExitCode() == 0 && result.getExecutionException() == null;
            if (!compiled) {
                getLog().warn("The compilation failed: the running application keeps the previous classes");
            }
        } catch (Exception e) {
            getLog().warn("Cannot compile the project: " + e.getMessage());
            compiled = false;
        }
        if (compiled) {
            try {
                Files.createDirectories(trigger.getParent());
                Files.writeString(trigger, String.valueOf(System.currentTimeMillis()), StandardCharsets.UTF_8);
            } catch (IOException e) {
                getLog().warn("Cannot touch " + trigger + ": " + e.getMessage());
            }
        }
    }

    /**
     * Whether a reactor project is the runnable one or among what it depends on, transitively: the
     * resolved artifacts hold the whole closure, and every reactor project in it is reloadable, since
     * the runtime classpath leaves all of them out.
     */
    private boolean isDependencyOfRunnableProject(MavenProject mavenProject) {
        return mavenProject.equals(runnableProject) || runnableProject.getArtifacts().stream()
            .anyMatch(artifact -> artifact.getGroupId().equals(mavenProject.getGroupId()) && artifact.getArtifactId().equals(mavenProject.getArtifactId()));
    }

    private static List<File> files(List<Dependency> dependencies) {
        List<File> files = new ArrayList<>(dependencies.size());
        for (Dependency dependency : dependencies) {
            File file = dependency.getArtifact().getFile();
            if (file != null) {
                files.add(file);
            }
        }
        return files;
    }

    private static List<String> translateArguments(String arguments) throws MojoExecutionException {
        if (arguments == null || arguments.isBlank()) {
            return List.of();
        }
        try {
            return Arrays.asList(CommandLineUtils.translateCommandline(arguments));
        } catch (Exception e) {
            throw new MojoExecutionException("Cannot parse the arguments: " + arguments, e);
        }
    }

    private void killProcess() {
        Process running = process;
        if (running != null && running.isAlive()) {
            running.destroy();
            try {
                running.waitFor();
            } catch (InterruptedException e) {
                running.destroyForcibly();
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * The test resources server of the run, started before the launch and stopped after it.
     */
    private final class TestResourcesHelperAccess {
        private final io.micronaut.maven.testresources.TestResourcesHelper helper = new io.micronaut.maven.testresources.TestResourcesHelper(
            testResourcesEnabled, shared, new File(runnableProject.getBuild().getDirectory()), explicitPort, clientTimeout, serverIdleTimeoutMinutes, runnableProject, mavenSession,
            dependencyResolutionService, toolchainManager, testResourcesVersion, classpathInference, testResourcesDependencies,
            sharedServerNamespace, debugServer, false, testResourcesSystemProperties);

        io.micronaut.maven.testresources.TestResourcesHelper helper() {
            return helper;
        }

        void start() throws MojoExecutionException {
            helper.start();
        }

        void stop() throws MojoExecutionException {
            helper.stop(true);
        }
    }
}
