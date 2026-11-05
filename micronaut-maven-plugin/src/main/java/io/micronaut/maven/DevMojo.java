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

import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.dev.BuildToolWatcher;
import io.micronaut.maven.dev.ControlPanel;
import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.dev.OutputLock;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import io.micronaut.maven.testresources.AbstractTestResourcesMojo;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.lifecycle.internal.MojoDescriptorCreator;
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
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.cli.CommandLineUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.artifact.ArtifactTypeRegistry;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static io.micronaut.maven.core.MojoUtils.findJavaExecutable;
import static io.micronaut.maven.core.MojoUtils.hasMicronautMavenPlugin;

/**
 * Runs the application in development mode through {@code io.micronaut.dev.MicronautDevMain}, the
 * launcher of Micronaut Core's {@code micronaut-dev} module: sources are compiled inside the running
 * JVM and the application is restarted on a fresh class loader, with the singletons a retention
 * policy names kept alive, or, for an edit that only changed method bodies, the classes are
 * redefined in place. This goal replaces {@code mn:run}, which is deprecated for removal in Micronaut 6.
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

    /**
     * The group of the launcher and its modules.
     */
    protected static final String LAUNCHER_GROUP = "io.micronaut";

    private static final String LAUNCHER_ARTIFACT = "micronaut-dev";
    private static final String LIVERELOAD_ARTIFACT = "micronaut-dev-livereload";
    private static final String TEST_RESOURCES_CLIENT = "micronaut-test-resources-client";
    private static final String MAC_WATCH_ARTIFACT = "micronaut-runtime-osx";
    /**
     * The status the launcher exits with when its generation budget, {@code micronaut.dev.max-generations}, is spent:
     * {@code MicronautDevMain.RELAUNCH}, for whoever started it to start it again.
     */
    private static final int RELAUNCH_STATUS = 3;
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
    @Parameter(defaultValue = MojoUtils.EXEC_MAIN_CLASS)
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
     * Whether Micronaut Control Panel is added to the development runtime: its UI and management panels, the testing
     * panels, and the panels of the modules the application uses, such as Kafka's, resolved through the dependency
     * management. The project's own classpath and packaging do not change.
     */
    @Parameter(property = "mn.dev.controlPanel", defaultValue = "true")
    private boolean controlPanel;

    /**
     * Whether the application listens on {@code localhost} by default, so that the development server is not
     * reachable from other machines. Written to the manifest as {@code micronaut.dev.bind-loopback}, which the
     * launcher applies at the lowest precedence: a {@code micronaut.server.host} the application sets in its
     * configuration, as an environment variable or as a system property wins. Set it to false to reach the
     * application from a phone or a container.
     */
    @Parameter(property = "mn.dev.bindLoopback", defaultValue = "true")
    private boolean bindLoopback;

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
    @Parameter(property = MojoUtils.MN_APP_ARGS)
    private String appArguments;

    @Parameter(defaultValue = "${mojoExecution}", readonly = true, required = true)
    private MojoExecution mojoExecution;

    private MavenProject runnableProject;
    private List<Dependency> runtimeDependencies;
    private List<File> controlPanelClasspath;
    /**
     * The application's process, read by the shutdown hook and the goal's own thread.
     */
    private final AtomicReference<Process> process = new AtomicReference<>();
    /**
     * Set when the build is shutting down, Ctrl+C for one: the application is stopped, and the status it
     * exits with is not a failure.
     */
    private volatile boolean stopping;
    /**
     * Set once the goal finishes: a compilation the watcher started no longer touches the trigger.
     */
    private volatile boolean finished;

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
        // a command line goal, mn:dev@id included, has no phase; only an execution the lifecycle runs does
        String phase = mojoExecution.getLifecyclePhase();
        if (phase != null) {
            throw new MojoExecutionException("mn:%s cannot be bound to a lifecycle phase, as it is here to %s: remove the execution and run it from the command line, mvn mn:%s"
                .formatted(mojoExecution.getGoal(), phase, mojoExecution.getGoal()));
        }
        runnableProject = findRunnableProject();
        dependencyResolutionService = new DependencyResolutionService(mavenSession, runnableProject, repositorySystem);
        if (!runnableProject.equals(mavenSession.getCurrentProject())) {
            applyRunnableProjectConfiguration();
        }
        Path manifestDirectory = manifestDirectory();
        Path manifestFile;
        try {
            manifestFile = buildManifest(manifestDirectory).writeTo(manifestDirectory);
        } catch (IOException e) {
            throw new MojoExecutionException("Cannot write the development mode manifest", e);
        }
        getLog().info("Development mode manifest written to " + manifestFile);
        if (manifestOnly) {
            return;
        }
        TestResourcesHelperAccess testResources = new TestResourcesHelperAccess();
        BuildToolWatcher watcher = null;
        Thread shutdownHook = null;
        try {
            testResources.start();
            List<String> args = launchArguments(manifestFile);
            if (getLog().isDebugEnabled()) {
                getLog().debug("Running " + String.join(" ", args));
            }
            ProcessBuilder launch = new ProcessBuilder(args).inheritIO().directory(runnableProject.getBasedir());
            Process started = launch.start();
            process.set(started);
            shutdownHook = new Thread(() -> {
                stopping = true;
                killProcess();
            });
            Runtime.getRuntime().addShutdownHook(shutdownHook);
            if (isBuildToolMode()) {
                watcher = watchAndCompile(manifestDirectory.resolve(DevManifest.TRIGGER_FILE_NAME));
            }
            int exit = awaitExit(started, launch);
            if (exit != 0 && !stopping) {
                // the application failed to start, or stopped on an error: the build says so
                throw new MojoExecutionException(exitFailure(exit));
            }
        } catch (InterruptedException _) {
            Thread.currentThread().interrupt();
        } catch (IOException | DependencyResolutionException e) {
            throw new MojoExecutionException("Cannot launch the development runtime: " + e.getMessage(), e);
        } finally {
            // in a Maven host that outlives the goal, such as mvnd, nothing of it keeps watching or compiling
            finished = true;
            if (watcher != null) {
                watcher.close();
            }
            killProcess();
            testResources.stop();
            removeShutdownHook(shutdownHook);
        }
    }

    /**
     * @return the project that runs, selected among the reactor
     */
    protected MavenProject runnableProject() {
        return runnableProject;
    }

    /**
     * The directory the manifest and its argument files are written to.
     *
     * @return {@code target/micronaut-dev}
     */
    protected Path manifestDirectory() {
        return Path.of(runnableProject.getBuild().getDirectory()).resolve(DevManifest.DIRECTORY_NAME);
    }

    /**
     * The message the build fails with when the launcher exits with a nonzero status.
     *
     * @param exit the status
     * @return the message
     */
    protected String exitFailure(int exit) {
        return "The application exited with status " + exit;
    }

    /**
     * The dependency scopes of the runtime classpath, which the launcher's JVM runs with.
     *
     * @return the scopes
     */
    protected String[] runtimeScopes() {
        return new String[] {JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME};
    }

    /**
     * The runtime classpath, the modules only: the reactor projects' outputs are reloadable.
     *
     * @return the dependencies, resolved once
     */
    protected List<Dependency> runtimeDependencies() {
        if (runtimeDependencies == null) {
            runtimeDependencies = compilerService.resolveDependencies(runnableProject, true, runtimeScopes());
        }
        return runtimeDependencies;
    }

    /**
     * An evaluator of the runnable project's configuration: its expressions resolve against it, not against
     * the reactor root an aggregator invocation started from.
     *
     * @return the evaluator
     */
    protected PluginParameterExpressionEvaluator runnableEvaluator() {
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
     * parameters, and merged over the goal's own defaults and property expressions, as Maven merges them: those
     * are evaluated against the application too, so that a property the application sets and the reactor root
     * does not, such as {@code micronaut.test.resources.enabled} or {@code exec.mainClass}, applies.
     */
    private void applyRunnableProjectConfiguration() throws MojoExecutionException {
        PluginParameterExpressionEvaluator evaluator = runnableEvaluator();
        Xpp3Dom effective = effectiveConfiguration();
        MojoDescriptor descriptor = mojoExecution.getMojoDescriptor();
        Xpp3Dom own = new Xpp3Dom("configuration");
        if (effective != null) {
            for (Xpp3Dom child : effective.getChildren()) {
                // another goal's settings, such as mn:run's watches, are not this goal's to apply
                if (isParameter(descriptor, child.getName())) {
                    own.addChild(new Xpp3Dom(child));
                }
            }
        }
        Xpp3Dom configuration = Xpp3Dom.mergeXpp3Dom(own, MojoDescriptorCreator.convert(descriptor));
        try {
            ClassRealm realm = descriptor.getPluginDescriptor().getClassRealm();
            configurator.configureComponent(this, new XmlPlexusConfiguration(configuration), evaluator, realm);
        } catch (ComponentConfigurationException e) {
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
     *
     * @param paths the paths
     * @return the resolved processor path
     * @throws DependencyResolutionException if a path cannot be resolved
     */
    protected List<File> resolveProcessorPath(DevManifest.ProcessorPaths paths) throws DependencyResolutionException {
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

    /**
     * Builds the manifest from the project model and the goal's settings.
     *
     * @param manifestDirectory the directory the manifest is written to
     * @return the manifest
     * @throws MojoExecutionException if it cannot be built
     */
    protected DevManifest buildManifest(Path manifestDirectory) throws MojoExecutionException {
        PluginParameterExpressionEvaluator evaluator = runnableEvaluator();
        List<File> runtimeClasspath = new ArrayList<>(files(runtimeDependencies()));
        runtimeClasspath.addAll(controlPanelClasspath());
        List<File> compileClasspath = files(compilerService.resolveDependencies(runnableProject, true, JavaScopes.PROVIDED, JavaScopes.COMPILE));
        List<File> processorPath;
        try {
            DevManifest.ProcessorPaths paths = DevManifest.processorPaths(runnableProject, evaluator, artifactTypes());
            processorPath = paths.dependencies().isEmpty() ? List.of() : resolveProcessorPath(paths);
        } catch (Exception e) {
            throw new MojoExecutionException("Cannot resolve the annotation processor path: " + e.getMessage(), e);
        }
        List<MavenProject> reactor = reactorDependencies();
        String main = mainClass != null ? mainClass : runnableProject.getProperties().getProperty("exec.mainClass");
        if (main == null && requiresMainClass()) {
            throw new MojoExecutionException("No main class: set exec.mainClass or the mainClass parameter");
        }
        List<String> retained = retain == null || retain.isBlank() ? List.of() : Arrays.stream(retain.split(",")).map(String::trim).filter(s -> !s.isEmpty()).toList();
        DevManifest.Settings settings = new DevManifest.Settings(main, strategy, compile, incremental, retained, liveReloadPort(), liveReloadInjectScript);
        DevManifest manifest = DevManifest.of(runnableProject, reactor, settings, runtimeClasspath, compileClasspath, processorPath, evaluator, manifestDirectory);
        return launcherBindsServer() ? manifest.withBindLoopback(bindLoopback) : manifest;
    }

    /**
     * Whether the launcher binds the application's HTTP server, which the {@code bindLoopback} parameter applies to.
     *
     * @return true, since the application runs
     */
    protected boolean launcherBindsServer() {
        return true;
    }

    /**
     * The port the LiveReload server listens on.
     *
     * @return the {@code liveReloadPort} parameter
     */
    protected int liveReloadPort() {
        return liveReloadPort;
    }

    /**
     * Whether the launch needs the application's main class.
     *
     * @return true, since the application runs
     */
    protected boolean requiresMainClass() {
        return true;
    }

    /**
     * The reactor projects the runnable one depends on, itself included, whose outputs are reloadable.
     *
     * @return the projects
     */
    protected List<MavenProject> reactorDependencies() {
        return mavenSession.getAllProjects().stream().filter(this::isDependencyOfRunnableProject).toList();
    }

    /**
     * Waits for the launcher to exit, and starts it again whenever it exits because its generation budget is spent,
     * as it asks whoever started it to: the new process starts from the classes the last change compiled.
     *
     * @param started the launcher's process
     * @param launch what starts it again
     * @return the status of the last process
     */
    private int awaitExit(Process started, ProcessBuilder launch) throws IOException, InterruptedException {
        int exit = started.waitFor();
        while (exit == RELAUNCH_STATUS && !stopping) {
            getLog().info("The development runtime spent its generation budget, micronaut.dev.max-generations: launching it again");
            Process relaunched = launch.start();
            process.set(relaunched);
            // the shutdown hook may have run while the old process was still the one it could stop
            if (stopping) {
                killProcess();
            }
            exit = relaunched.waitFor();
        }
        return exit;
    }

    private List<String> launchArguments(Path manifestFile) throws MojoExecutionException, DependencyResolutionException {
        List<ArtifactResult> launcherResults = dependencyResolutionService.artifactResultsFor(launcherArtifacts().stream(), true);
        List<File> launcher = DependencyResolutionService.toClasspathFiles(launcherResults);
        // the launcher itself, not merely any of the artifacts: one with a version of its own resolves without it
        org.eclipse.aether.artifact.Artifact launcherArtifact = launcherResults.stream()
            .map(ArtifactResult::getArtifact)
            .filter(artifact -> LAUNCHER_GROUP.equals(artifact.getGroupId()) && LAUNCHER_ARTIFACT.equals(artifact.getArtifactId()))
            .findFirst()
            .orElse(null);
        if (launcherArtifact == null) {
            throw new MojoExecutionException("Cannot resolve " + LAUNCHER_GROUP + ":" + LAUNCHER_ARTIFACT + ": development mode needs Micronaut Core 5.3 or later in the dependency management");
        }
        List<File> runtime = files(runtimeDependencies());
        // the runtime classpath, then what of the Control Panel, the launcher and the test resources client it does not hold already
        List<String> classpath = Stream.of(runtime, controlPanelClasspath(), launcher, testResourcesClient())
            .flatMap(List::stream)
            .map(File::getAbsolutePath)
            .distinct()
            .toList();
        List<String> translatedJvmArguments = translateArguments(jvmArguments);
        List<String> args = new ArrayList<>();
        args.add(javaExecutable);
        if (debug) {
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + (debugSuspend ? "y" : "n") + ",address=" + debugHost + ":" + debugPort);
        }
        addTestResourcesArguments(args);
        args.addAll(translatedJvmArguments);
        mavenSession.getUserProperties().forEach((key, value) -> args.add("-D" + key + "=" + value));
        // JNA loads its library for the native macOS watch service, which the JVM warns about otherwise
        if (isMac()) {
            args.add("--enable-native-access=ALL-UNNAMED");
        }
        // the launcher as the JVM's agent, for the method-body fast path
        args.add("-javaagent:" + launcherArtifact.getFile().getAbsolutePath());
        args.add("-classpath");
        args.add(String.join(File.pathSeparator, classpath));
        args.add(LAUNCHER_MAIN_CLASS);
        args.add("--manifest");
        args.add(manifestFile.toString());
        args.addAll(translateArguments(appArguments));
        return args;
    }

    /**
     * @return the session's artifact types, which map the {@code type} of a processor path to its artifact
     */
    protected ArtifactTypeRegistry artifactTypes() {
        return mavenSession.getRepositorySession().getArtifactTypeRegistry();
    }

    /**
     * The artifacts the launcher's JVM runs with beside the runtime classpath, versioned by the dependency
     * management when they name no version.
     *
     * @return the launcher, and the LiveReload module when enabled
     */
    protected List<org.eclipse.aether.artifact.Artifact> launcherArtifacts() {
        List<org.eclipse.aether.artifact.Artifact> artifacts = new ArrayList<>();
        // versioned by the dependency management, the platform BOM the parent imports
        artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, LAUNCHER_ARTIFACT, "jar", ""));
        if (liveReload) {
            artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, LIVERELOAD_ARTIFACT, "jar", ""));
        }
        // FSEvents through JNA, which the launcher uses when it finds it: without it, macOS has only polling
        if (isMac()) {
            artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, MAC_WATCH_ARTIFACT, "jar", ""));
        }
        return artifacts;
    }

    /**
     * Whether Micronaut Control Panel is added to the development runtime.
     *
     * @return the {@code controlPanel} parameter
     */
    protected boolean controlPanelEnabled() {
        return controlPanel;
    }

    /**
     * The Control Panel the development runtime adds, resolved once: the modules {@link ControlPanel#modules(java.util.Collection)}
     * infers from the runtime classpath, each with what it brings that the runtime classpath does not hold, so that the
     * application's versions win, versioned by the runnable project's dependency management, the Micronaut platform BOM;
     * then the directory holding {@link ControlPanel#PROPERTIES_RESOURCE}, the settings at the lowest precedence. A
     * Control Panel module that is not versioned or not published, such as the testing panels before their release,
     * is left out; any other resolution failure fails the goal. When the Control Panel is off, the directory is removed.
     *
     * @return the files to add to the runtime classpath
     * @throws MojoExecutionException if the modules cannot be resolved or the settings cannot be written
     */
    private List<File> controlPanelClasspath() throws MojoExecutionException {
        if (controlPanelClasspath != null) {
            return controlPanelClasspath;
        }
        Set<String> present = runtimeModuleKeys();
        ControlPanel.Additions additions = new ControlPanel.Additions(present);
        if (controlPanelEnabled() && platformSupportsControlPanel()) {
            for (String module : ControlPanel.modules(present)) {
                addControlPanelModule(module, additions);
            }
        }
        List<File> files = new ArrayList<>(additions.files());
        Path resources = manifestDirectory().resolve(ControlPanel.RESOURCES_DIRECTORY_NAME);
        updateControlPanelSettings(resources, additions.active(), present);
        if (additions.active()) {
            files.add(resources.toFile());
        }
        if (!additions.added().isEmpty()) {
            getLog().info("Adding Micronaut Control Panel to the development runtime: " + additions.added() + " (disable it with mn.dev.controlPanel=false)");
        }
        controlPanelClasspath = files;
        return controlPanelClasspath;
    }

    /**
     * @return the {@code group:artifact} keys of the runtime classpath
     */
    private Set<String> runtimeModuleKeys() {
        Set<String> keys = new LinkedHashSet<>();
        for (Dependency dependency : runtimeDependencies()) {
            org.eclipse.aether.artifact.Artifact artifact = dependency.getArtifact();
            keys.add(artifact.getGroupId() + ":" + artifact.getArtifactId());
        }
        return keys;
    }

    /**
     * Adds a Control Panel module, unless the runtime classpath holds it already. One that is not published or not
     * versioned by the dependency management is left out, and one line says so.
     *
     * @param module the artifact id of the module
     * @param additions what the runtime adds so far
     * @throws MojoExecutionException if the module cannot be resolved for any other reason
     */
    private void addControlPanelModule(String module, ControlPanel.Additions additions) throws MojoExecutionException {
        if (additions.holds(module)) {
            return;
        }
        String key = ControlPanel.GROUP + ":" + module;
        try {
            List<ArtifactResult> results = dependencyResolutionService.artifactResultsFor(Stream.of(new DefaultArtifact(ControlPanel.GROUP, module, "jar", "")), true);
            if (!additions.add(module, results)) {
                getLog().info("Micronaut Control Panel module " + key + " is not versioned by the dependency management: development mode runs without it");
            }
        } catch (DependencyResolutionException e) {
            if (!ControlPanel.isMissingControlPanelArtifact(e)) {
                throw new MojoExecutionException("Cannot resolve the Micronaut Control Panel module " + key + " (disable it with mn.dev.controlPanel=false): " + e.getMessage(), e);
            }
            getLog().info("Micronaut Control Panel module " + key + " is not published: development mode runs without it");
        }
    }

    /**
     * Writes the Control Panel's settings when it is active, and removes those of an earlier run when it is not.
     *
     * @param resources the directory of the settings
     * @param active whether the development runtime has a Control Panel
     * @param present the {@code group:artifact} keys of the runtime classpath
     * @throws MojoExecutionException if the settings cannot be written or removed
     */
    private static void updateControlPanelSettings(Path resources, boolean active, Set<String> present) throws MojoExecutionException {
        try {
            if (active) {
                ControlPanel.writeResources(resources, present);
            } else {
                ControlPanel.deleteResources(resources);
            }
        } catch (IOException e) {
            throw new MojoExecutionException("Cannot write the Micronaut Control Panel settings to " + resources + ": " + e.getMessage(), e);
        }
    }

    /**
     * Whether the runnable project builds with a Micronaut platform Control Panel is added for: Control Panel requires
     * Micronaut Platform {@value ControlPanel#MINIMUM_PLATFORM_VERSION} or later. Otherwise one line says why it is
     * left out.
     *
     * @return whether the platform is {@value ControlPanel#MINIMUM_PLATFORM_VERSION} or later
     */
    private boolean platformSupportsControlPanel() {
        Optional<String> version = ControlPanel.platformVersion(runnableProject());
        if (version.isPresent() && ControlPanel.isSupportedPlatform(version.get())) {
            return true;
        }
        getLog().warn("Micronaut Control Panel requires Micronaut Platform " + ControlPanel.MINIMUM_PLATFORM_VERSION
            + " or later; not adding it (the build uses " + version.orElse("no Micronaut Platform") + ")");
        return false;
    }

    /**
     * The Test Resources client, which resolves the application's properties through the server, when Test Resources
     * is enabled and the runtime classpath lacks it: an application that declares it, usually in the
     * {@code provided} scope, launches with its own. It is resolved at the version of the server, and only the
     * modules the runtime classpath does not hold are added, so that the application's versions win.
     *
     * @return the client and the modules it brings, none when it is not needed
     * @throws DependencyResolutionException if the client cannot be resolved
     */
    private List<File> testResourcesClient() throws DependencyResolutionException {
        if (!testResourcesEnabled) {
            return List.of();
        }
        List<String> present = new ArrayList<>();
        for (Dependency dependency : runtimeDependencies()) {
            org.eclipse.aether.artifact.Artifact artifact = dependency.getArtifact();
            present.add(artifact.getGroupId() + ":" + artifact.getArtifactId());
        }
        String client = DependencyResolutionService.TEST_RESOURCES_GROUP + ":" + TEST_RESOURCES_CLIENT;
        if (present.contains(client)) {
            return List.of();
        }
        String version = testResourcesVersion == null ? "" : testResourcesVersion;
        List<ArtifactResult> results = dependencyResolutionService.artifactResultsFor(
            Stream.of(new DefaultArtifact(DependencyResolutionService.TEST_RESOURCES_GROUP, TEST_RESOURCES_CLIENT, "jar", version)), true);
        List<File> files = new ArrayList<>();
        for (ArtifactResult result : results) {
            org.eclipse.aether.artifact.Artifact artifact = result.getArtifact();
            if (artifact != null && artifact.getFile() != null && !present.contains(artifact.getGroupId() + ":" + artifact.getArtifactId())) {
                files.add(artifact.getFile());
            }
        }
        getLog().info("Adding " + client + (version.isEmpty() ? "" : ":" + version) + " to the launch classpath: Test Resources is enabled and the application does not declare the client");
        return files;
    }

    private static boolean isMac() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac");
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
     * finished, so that it applies the new classes. The watcher lives until the goal finishes.
     *
     * @return the watcher, or null when there is nothing to watch
     */
    private BuildToolWatcher watchAndCompile(Path trigger) throws IOException {
        List<Path> paths = new ArrayList<>();
        for (MavenProject project : mavenSession.getAllProjects()) {
            if (!isDependencyOfRunnableProject(project)) {
                continue;
            }
            // the roots as configured, a build-helper or a custom resource directory included
            for (String root : watchedRoots(project)) {
                Path directory = Path.of(root);
                if (Files.isDirectory(directory)) {
                    paths.add(directory);
                }
            }
        }
        if (paths.isEmpty()) {
            return null;
        }
        BuildToolWatcher watcher = BuildToolWatcher.start(paths, QUIET_PERIOD_MILLIS, () -> compileAndTouch(trigger));
        getLog().info("👀 Watching for changes in " + paths.stream().map(path -> runnableProject.getBasedir().toPath().relativize(path).toString()).toList());
        return watcher;
    }

    /**
     * The roots watched in build-tool mode for a reactor project.
     *
     * @param project the project
     * @return its source and resource roots, which may not exist
     */
    protected List<String> watchedRoots(MavenProject project) {
        return DevManifest.sourceRoots(project);
    }

    /**
     * The goal that compiles the project in build-tool mode.
     *
     * @return {@code compile}
     */
    protected String buildToolGoal() {
        return "compile";
    }

    /**
     * Whether a build-tool mode compilation writes the test output too, whose lock it then takes as well.
     *
     * @return false, {@code compile} writes the application's output only
     */
    protected boolean compilesTests() {
        return false;
    }

    private void compileAndTouch(Path trigger) {
        if (finished) {
            return;
        }
        // the invocation's exit code, not merely its return: a compiler error is a normal result of the invoker
        boolean compiled;
        try {
            MavenProject root = executionRoot();
            InvocationResult result;
            // the launcher of mn:test or mn:dev running beside this goal compiles into the same output: never at once
            try (OutputLock.Hold hold = OutputLock.lock(OutputLock.files(runnableProject, compilesTests()), getLog()::info)) {
                result = executorService.invokeGoals(root, runnableProject, buildToolGoal());
            }
            compiled = result.getExitCode() == 0 && result.getExecutionException() == null;
            if (!compiled) {
                getLog().warn("The compilation failed: the running application keeps the previous classes");
            }
        } catch (Exception e) {
            if (!finished) {
                getLog().warn("Cannot compile the project: " + e.getMessage());
            }
            compiled = false;
        }
        if (compiled && !finished) {
            try {
                Files.createDirectories(trigger.getParent());
                Files.writeString(trigger, String.valueOf(System.currentTimeMillis()), StandardCharsets.UTF_8);
            } catch (IOException e) {
                getLog().warn("Cannot touch " + trigger + ": " + e.getMessage());
            }
        }
    }

    /**
     * The project whose POM the build was started from: its reactor holds the runnable project and what it depends
     * on, whereas the top level project of the session is the first selected project when {@code -pl} leaves the
     * execution root out.
     */
    private MavenProject executionRoot() {
        return mavenSession.getAllProjects().stream()
            .filter(MavenProject::isExecutionRoot)
            .findFirst()
            .orElseGet(mavenSession::getTopLevelProject);
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

    /**
     * The files of resolved dependencies.
     *
     * @param dependencies the dependencies
     * @return their files
     */
    protected static List<File> files(List<Dependency> dependencies) {
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

    private static void removeShutdownHook(Thread shutdownHook) {
        if (shutdownHook != null) {
            try {
                Runtime.getRuntime().removeShutdownHook(shutdownHook);
            } catch (IllegalStateException _) {
                // the JVM is shutting down: the hook runs
            }
        }
    }

    private void killProcess() {
        Process running = process.get();
        if (running != null && running.isAlive()) {
            running.destroy();
            try {
                running.waitFor();
            } catch (InterruptedException _) {
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
