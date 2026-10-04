/*
 * Copyright 2017-2022 original authors
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
import io.micronaut.core.annotation.Experimental;
import io.micronaut.maven.aot.AbstractAotAnalysisMojo;
import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import io.micronaut.maven.jsonschema.ValidateDevConfigurationMojo;
import io.micronaut.maven.testresources.AbstractTestResourcesMojo;
import io.micronaut.maven.testresources.TestResourcesHelper;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.lifecycle.internal.MojoDescriptorCreator;
import org.apache.maven.model.Build;
import org.apache.maven.model.FileSet;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.classworlds.realm.ClassRealm;
import org.codehaus.plexus.component.configurator.ComponentConfigurationException;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.AbstractScanner;
import org.codehaus.plexus.util.cli.CommandLineUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;

import static io.micronaut.maven.core.MojoUtils.findJavaExecutable;
import static io.micronaut.maven.core.MojoUtils.hasMicronautMavenPlugin;
import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static java.nio.file.Files.isDirectory;
import static java.nio.file.Files.isReadable;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;

/**
 * <p>Executes a Micronaut application in development mode.</p>
 *
 * <p>It watches for changes in the project tree. If there are changes in the {@code pom.xml} file, dependencies will be reloaded. If
 * the changes are anywhere underneath {@code src/main}, it will recompile the project and restart the application.</p>
 *
 * <p>The plugin can handle changes in Java, Groovy and resource source directories by default.</p>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Mojo(name = "run", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE, aggregator = true)
@Execute(phase = LifecyclePhase.PROCESS_CLASSES)
public class RunMojo extends AbstractTestResourcesMojo {

    public static final String MN_APP_ARGS = "mn.appArgs";
    public static final String EXEC_MAIN_CLASS = "${exec.mainClass}";
    public static final String RESOURCES_DIR = "src/main/resources";

    private static final List<String> RELEVANT_SRC_DIRS = List.of("resources", "java", "groovy");
    private static final int LAST_COMPILATION_THRESHOLD = 500;
    private static final String DEFAULT_CLI_EXECUTION = "default-cli";
    private static final List<String> DEFAULT_EXCLUDES;

    static {
        DEFAULT_EXCLUDES = new ArrayList<>();
        Collections.addAll(DEFAULT_EXCLUDES, AbstractScanner.DEFAULTEXCLUDES);
        Collections.addAll(DEFAULT_EXCLUDES, "**/.idea/**", "**/src/test/**");
    }

    private final MavenSession mavenSession;
    private final ProjectBuilder projectBuilder;
    private final ToolchainManager toolchainManager;
    private final String javaExecutable;
    private final DependencyResolutionService dependencyResolutionService;
    private final CompilerService compilerService;
    private final ExecutorService executorService;
    private final ComponentConfigurator configurator;

    /**
     * The project's target directory.
     */
    private File targetDirectory;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = EXEC_MAIN_CLASS)
    private String mainClass;

    /**
     * Whether to start the Micronaut application in debug mode.
     */
    @Parameter(property = "mn.debug", defaultValue = "false")
    private boolean debug;

    /**
     * Whether to suspend the execution of the application when running in debug mode.
     */
    @Parameter(property = "mn.debug.suspend", defaultValue = "false")
    private boolean debugSuspend;

    /**
     * The port where remote debuggers can be attached to.
     */
    @Parameter(property = "mn.debug.port", defaultValue = "5005")
    private int debugPort;

    /**
     * The host where remote debuggers can connect.
     */
    @Parameter(property = "mn.debug.host", defaultValue = "127.0.0.1")
    private String debugHost;

    /**
     * List of inclusion/exclusion paths that should not trigger an application restart.
     * For example, you can exclude a particular directory from being watched by adding the following
     * configuration:
     * <pre>
     *     &lt;watches&gt;
     *         &lt;watch&gt;
     *             &lt;directory&gt;.some-dir&lt;/directory&gt;
     *             &lt;excludes&gt;
     *                 &lt;exclude&gt;**&#47;*&lt;/exclude&gt;
     *             &lt;/excludes&gt;
     *         &lt;/watch&gt;
     *     &lt;/watches&gt;
     * </pre>
     * Check the
     * <a href="https://maven.apache.org/ref/3.3.9/maven-model/apidocs/org/apache/maven/model/FileSet.html">FileSet</a>
     * documentation for more details.
     *
     * @see <a href="https://maven.apache.org/ref/3.3.9/maven-model/apidocs/org/apache/maven/model/FileSet.html">FileSet</a>
     */
    @Parameter
    private List<FileSet> watches;

    /**
     * <p>List of additional arguments that will be passed to the JVM process, such as Java agent properties.</p>
     *
     * <p>When using the command line, user properties will be passed through, eg: <code>mnv mn:run -Dmicronaut.environments=dev</code>.</p>
     */
    @Parameter(property = "mn.jvmArgs")
    private String jvmArguments;

    /**
     * List of additional arguments that will be passed to the application, after the class name.
     */
    @Parameter(property = MN_APP_ARGS)
    private String appArguments;

    /**
     * Whether to watch for changes, or finish the execution after the first run.
     */
    @Parameter(property = "mn.watch", defaultValue = "true")
    private boolean watchForChanges;

    /**
     * Whether to enable or disable Micronaut AOT.
     */
    @Parameter(property = "micronaut.aot.enabled", defaultValue = "false")
    private boolean aotEnabled;

    /**
     * <p><b>Experimental.</b> Whether to keep the classes of the dependency JARs in a static CDS (class data sharing)
     * archive, so that the launches and restarts after the first one load them from the archive instead of parsing
     * and verifying them again. It needs JDK 25 or later.</p>
     *
     * <p>The first launch records the classes it loads. When it ends, the archive is created in the background under
     * {@code target/mn-cds}, and later launches and restarts use it until the dependencies, the JDK or the JVM options
     * change. While this option is on, the class path starts with the dependency JARs, followed by the
     * {@code target/classes} directories of this project and of the reactor projects it depends on, which stay outside
     * the archive.</p>
     *
     * @since 5.1.0
     */
    @Experimental
    @Parameter(property = "mn.classDataSharing", defaultValue = "false")
    private boolean classDataSharing;

    @Parameter(defaultValue = "${mojoExecution}", readonly = true, required = true)
    private MojoExecution mojoExecution;

    // These 2 flags are used in the context of watching for changes
    // the first one makes sure that only one recompilation is processed at a time
    private final AtomicBoolean recompileRequested = new AtomicBoolean();
    // the second one makes sure that we wait for the server to be started before we restart it
    // otherwise a process may be kept alive
    private final ReentrantLock restartLock = new ReentrantLock();

    private MavenProject runnableProject;
    private DirectoryWatcher directoryWatcher;
    private volatile Process process;
    private String classpath;
    private int classpathHash;
    private long lastCompilation;
    private TestResourcesHelper testResourcesHelper;
    private ClassDataSharingSupport classDataSharingSupport;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public RunMojo(MavenSession mavenSession,
                   BuildPluginManager pluginManager,
                   ProjectBuilder projectBuilder,
                   ToolchainManager toolchainManager,
                   CompilerService compilerService,
                   ExecutorService executorService,
                   DependencyResolutionService dependencyResolutionService,
                   @Named("basic") ComponentConfigurator configurator) {
        this.mavenSession = mavenSession;
        this.projectBuilder = projectBuilder;
        this.toolchainManager = toolchainManager;
        this.compilerService = compilerService;
        this.executorService = executorService;
        this.javaExecutable = findJavaExecutable(toolchainManager, mavenSession);
        this.dependencyResolutionService = dependencyResolutionService;
        this.configurator = configurator;
    }

    @Override
    public void execute() throws MojoExecutionException {
        try {
            initialize();
        } catch (Exception e) {
            throw new MojoExecutionException(e.getMessage());
        }

        try {
            maybeStartTestResourcesServer();
            runApplication();
            Thread shutdownHook = new Thread(this::stopOnShutdown);
            Runtime.getRuntime().addShutdownHook(shutdownHook);

            if (process != null && process.isAlive()) {
                if (watchForChanges) {
                    var pathsToWatch = new ArrayList<Path>();
                    for (FileSet fs : watches) {
                        var directory = runnableProject.getBasedir().toPath().resolve(fs.getDirectory()).toAbsolutePath();
                        if (Files.exists(directory)) {
                            pathsToWatch.add(directory);
                            //If neither includes nor excludes, add a default include
                            if ((fs.getIncludes() == null || fs.getIncludes().isEmpty()) && (fs.getExcludes() == null || fs.getExcludes().isEmpty())) {
                                fs.addInclude("**/*");
                            }
                        }
                    }

                    this.directoryWatcher = DirectoryWatcher
                        .builder()
                        .paths(pathsToWatch)
                        .listener(this::handleEvent)
                        .build();

                    // We use the working directory as the root path, because
                    // the top-level project information may not be what we
                    // expect, in particular if we run from a submodule, or
                    // that we run from root but with the "-pl" option.
                    // We can safely do this because it's only used to display
                    // information about paths being watched to the user, the
                    // actual paths are unchanged.
                    Path root = Path.of(".").toAbsolutePath();
                    List<Path> pathList = pathsToWatch.stream()
                        .map(root::relativize)
                        .filter(s -> !s.toString().isEmpty())
                        .filter(Files::exists)
                        .sorted()
                        .toList();
                    getLog().info("👀 Watching for changes in " + pathList);
                    this.directoryWatcher.watch();
                } else if (process != null && process.isAlive()) {
                    process.waitFor();
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            if (getLog().isDebugEnabled()) {
                getLog().debug("Exception while watching for changes", e);
            }
            throw new MojoExecutionException("Exception while watching for changes", e);
        } finally {
            killProcess();
            if (classDataSharingSupport != null) {
                classDataSharingSupport.awaitBackgroundWork();
            }
            cleanup();
        }
    }

    protected final void initialize() {
        final MavenProject currentProject = mavenSession.getCurrentProject();
        if (hasMicronautMavenPlugin(currentProject)) {
            runnableProject = currentProject;
        } else {
            final List<MavenProject> projectsWithPlugin = mavenSession.getProjects().stream()
                .filter(MojoUtils::hasMicronautMavenPlugin)
                .toList();
            if (projectsWithPlugin.size() == 1) {
                runnableProject = projectsWithPlugin.get(0);
                log.info("Running project %s".formatted(runnableProject.getArtifactId()));
            } else {
                throw new IllegalStateException("The Micronaut Maven Plugin is declared in the following projects: %s. Please specify the project to run with the -pl option."
                    .formatted(projectsWithPlugin.stream().map(MavenProject::getArtifactId).toList()));
            }
            applyRunnableProjectConfiguration();
        }
        this.targetDirectory = new File(runnableProject.getBuild().getDirectory());
        if (classDataSharing) {
            this.classDataSharingSupport = new ClassDataSharingSupport(getLog(),
                targetDirectory.toPath().resolve(ClassDataSharingSupport.DIRECTORY_NAME), javaExecutable, System.getenv());
        }
        this.testResourcesHelper = new TestResourcesHelper(testResourcesEnabled, shared, buildDirectory, explicitPort,
                clientTimeout, serverIdleTimeoutMinutes, runnableProject, mavenSession, dependencyResolutionService,
                toolchainManager, testResourcesVersion, classpathInference, testResourcesDependencies,
                sharedServerNamespace, debugServer, false, testResourcesSystemProperties);
        resolveDependencies();
        if (watches == null) {
            watches = new ArrayList<>();
        }
        // watch pom.xml file changes
        mavenSession.getAllProjects().stream()
            .filter(this::isDependencyOfRunnableProject)
            .map(MavenProject::getBasedir)
            .map(File::toPath)
            .forEach(path -> {
                var fileSet = new FileSet();
                fileSet.setDirectory(path.toString());
                fileSet.addInclude("pom.xml");
                watches.add(fileSet);
            });
        // Add the default watch paths
        mavenSession.getAllProjects().stream()
            .filter(this::isDependencyOfRunnableProject)
            .flatMap(p -> {
                var basedir = p.getBasedir().toPath();
                return RELEVANT_SRC_DIRS.stream().map(dir -> basedir.resolve("src/main/" + dir));
            })
            .forEach(path -> {
                var fileSet = new FileSet();
                fileSet.setDirectory(path.toString());
                fileSet.addInclude("**/*");
                watches.add(fileSet);
            });

        compileProject();
    }

    /**
     * Configures this goal as the selected application declares it. Maven configured the mojo from the project the
     * goal was invoked on, the reactor root when {@code mn:run} runs from an aggregator that does not declare the
     * plugin, so a property-backed parameter, such as {@code micronaut.test.resources.enabled}, and the plugin's
     * configuration, such as {@code testResourcesDependencies}, came from the root. Here the application's
     * plugin-level {@code micronaut-maven-plugin} configuration, with its {@code default-cli} execution's merged over
     * it, restricted to this goal's parameters and merged over the goal's defaults and property expressions, is applied
     * with Maven's configurator and evaluated against the application, as Maven would have done had the goal run on it.
     */
    private void applyRunnableProjectConfiguration() {
        if (mojoExecution == null || configurator == null) {
            return;
        }
        MojoDescriptor descriptor = mojoExecution.getMojoDescriptor();
        Xpp3Dom configuration = Xpp3Dom.mergeXpp3Dom(runnableProjectConfiguration(runnableProject, descriptor),
            MojoDescriptorCreator.convert(descriptor));
        MavenSession runnableSession = mavenSession.clone();
        runnableSession.setCurrentProject(runnableProject);
        var evaluator = new PluginParameterExpressionEvaluator(runnableSession, mojoExecution);
        try {
            ClassRealm realm = descriptor.getPluginDescriptor().getClassRealm();
            configurator.configureComponent(this, new XmlPlexusConfiguration(configuration), evaluator, realm);
        } catch (ComponentConfigurationException e) {
            throw new IllegalStateException("Cannot apply the micronaut-maven-plugin configuration of " + runnableProject.getArtifactId() + ": " + e.getMessage(), e);
        }
    }

    /**
     * The configuration of this goal the project declares: its plugin-level {@code micronaut-maven-plugin}
     * configuration, with the {@code default-cli} execution's merged over it, restricted to the goal's parameters.
     *
     * @param project the project
     * @param descriptor the goal's descriptor
     * @return the configuration, empty when the project declares none
     */
    static Xpp3Dom runnableProjectConfiguration(MavenProject project, MojoDescriptor descriptor) {
        Xpp3Dom own = new Xpp3Dom("configuration");
        Plugin plugin = project.getPlugin(THIS_PLUGIN);
        if (plugin == null) {
            return own;
        }
        Xpp3Dom effective = plugin.getConfiguration() instanceof Xpp3Dom dom ? new Xpp3Dom(dom) : null;
        for (PluginExecution execution : plugin.getExecutions()) {
            if (DEFAULT_CLI_EXECUTION.equals(execution.getId()) && execution.getConfiguration() instanceof Xpp3Dom executionDom) {
                effective = effective == null ? new Xpp3Dom(executionDom) : Xpp3Dom.mergeXpp3Dom(new Xpp3Dom(executionDom), effective);
            }
        }
        if (effective != null) {
            for (Xpp3Dom child : effective.getChildren()) {
                // another goal's settings, such as the AOT goals', are not this goal's to apply
                if (isParameter(descriptor, child.getName())) {
                    own.addChild(new Xpp3Dom(child));
                }
            }
        }
        return own;
    }

    private static boolean isParameter(MojoDescriptor descriptor, String name) {
        if (descriptor.getParameterMap().containsKey(name)) {
            return true;
        }
        return descriptor.getParameters() != null && descriptor.getParameters().stream().anyMatch(parameter -> name.equals(parameter.getAlias()));
    }

    private boolean isDependencyOfRunnableProject(MavenProject mavenProject) {
        return mavenProject.equals(runnableProject) || runnableProject.getDependencies().stream()
            .anyMatch(d -> d.getGroupId().equals(mavenProject.getGroupId()) && d.getArtifactId().equals(mavenProject.getArtifactId()));
    }

    protected final void setWatches(List<FileSet> watches) {
        this.watches = watches;
    }

    final void handleEvent(DirectoryChangeEvent event) {
        Path path = event.path();
        Path parent = path.getParent();
        Path projectRootDirectory = mavenSession.getTopLevelProject().getBasedir().toPath();

        if (matches(path)) {
            if (getLog().isInfoEnabled()) {
                getLog().info(String.format("📝 Detected change in %s. Recompiling/restarting...", projectRootDirectory.relativize(path)));
            }
            boolean compiledOk = compileProject();
            if (compiledOk) {
                try {
                    runApplication();
                } catch (Exception e) {
                    getLog().error("Unable to run application: " + e.getMessage(), e);
                }
            }
        }
    }

    private boolean matches(Path path) {
        // Apply default exclusions
        if (isDefaultExcluded(path) || isDirectory(path, NOFOLLOW_LINKS) || !isReadable(path) || hasBeenCompiledRecently()) {
            return false;
        }

        Path projectRootDirectory = mavenSession.getTopLevelProject().getBasedir().toPath();

        String relativePath = projectRootDirectory.relativize(path).toString();

        boolean matches = false;
        for (FileSet fileSet : watches) {
            if (fileSet.getIncludes() != null && !fileSet.getIncludes().isEmpty()) {
                var directory = new File(fileSet.getDirectory());
                if (directory.exists() && path.getParent().startsWith(directory.getAbsolutePath())) {
                    for (String includePattern : fileSet.getIncludes()) {
                        if (pathMatches(includePattern, path) || patternEquals(path, includePattern, directory)) {
                            matches = true;
                            if (getLog().isDebugEnabled()) {
                                getLog().debug("Path [" + relativePath + "] matched the include pattern [" + includePattern + "] of the directory [" + fileSet.getDirectory() + "]");
                            }
                            break;
                        }
                    }
                }
            }
            if (matches) {
                break;
            }
        }

        // Finally, process excludes only if the path is matching
        if (matches) {
            for (FileSet fileSet : watches) {
                if (fileSet.getExcludes() != null && !fileSet.getExcludes().isEmpty()) {
                    File directory = new File(fileSet.getDirectory());
                    if (directory.exists() && path.getParent().startsWith(directory.getAbsolutePath())) {
                        for (String excludePattern : fileSet.getExcludes()) {
                            if (pathMatches(excludePattern, path) || patternEquals(path, excludePattern, directory)) {
                                matches = false;
                                if (getLog().isDebugEnabled()) {
                                    getLog().debug("Path [" + relativePath + "] matched the exclude pattern [" + excludePattern + "] of the directory [" + fileSet.getDirectory() + "]");
                                }
                                break;
                            }
                        }
                    }
                }
                if (!matches) {
                    break;
                }
            }
        }

        return matches;
    }

    private boolean isDefaultExcluded(Path path) {
        boolean excludeTargetDirectory = true;
        if (this.watches != null && !this.watches.isEmpty()) {
            for (FileSet fileSet : this.watches) {
                if (fileSet.getDirectory().equals(this.targetDirectory.getName())) {
                    excludeTargetDirectory = false;
                }
            }
        }
        return (excludeTargetDirectory && path.startsWith(targetDirectory.getAbsolutePath())) ||
            DEFAULT_EXCLUDES.stream()
                .anyMatch(excludePattern -> pathMatches(excludePattern, path));
    }

    private boolean hasBeenCompiledRecently() {
        return (System.currentTimeMillis() - lastCompilation) < LAST_COMPILATION_THRESHOLD;
    }

    private void cleanup() {
        if (getLog().isDebugEnabled()) {
            getLog().debug("Cleaning up");
        }
        try {
            directoryWatcher.close();
            maybeStopTestResourcesServer();
        } catch (Exception e) {
            // Do nothing
        }
    }

    private boolean rebuildMavenProject() {
        boolean success = true;
        try {
            ProjectBuildingRequest projectBuildingRequest = mavenSession.getProjectBuildingRequest();
            projectBuildingRequest.setResolveDependencies(true);
            ProjectBuildingResult build = projectBuilder.build(runnableProject.getArtifact(), projectBuildingRequest);
            MavenProject project = build.getProject();
            runnableProject = project;
            mavenSession.setCurrentProject(project);
        } catch (ProjectBuildingException e) {
            success = false;
            if (getLog().isWarnEnabled()) {
                getLog().warn("Error while trying to build the Maven project model", e);
            }
        }
        return success;
    }

    private boolean resolveDependencies() {
        try {
            List<Dependency> dependencies = compilerService.resolveDependencies(runnableProject, true, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME);
            if (dependencies.isEmpty()) {
                return false;
            } else {
                this.classpath = compilerService.buildClasspath(dependencies);
                return true;
            }
        } finally {
            if (classpath != null) {
                this.classpathHash = this.classpath.hashCode();
            }
        }
    }

    private boolean classpathHasChanged() {
        int oldClasspathHash = this.classpathHash;
        this.classpathHash = this.classpath.hashCode();
        return oldClasspathHash != classpathHash;

    }

    /**
     * Runs or restarts the application. Only visible for testing, shouldn't
     * be called directly.
     *
     * @throws Exception if something goes wrong while starting the application
     */
    protected void runApplication() throws Exception {
        if (restartLock.getQueueLength() >= 1) {
            // if there's more than one restart request, we'll handle them all at once
            return;
        }
        restartLock.lock();
        try {
            // Validate Micronaut configuration for the dev environment before starting.
            executorService.executeGoal(runnableProject, THIS_PLUGIN, ValidateDevConfigurationMojo.MOJO_NAME, configurationValidationConfiguration());
            runAotIfNeeded();
            List<String> args = buildRunArguments();

            if (getLog().isDebugEnabled()) {
                getLog().debug("Running " + String.join(" ", args));
            }

            killProcess();
            process = new ProcessBuilder(args)
                .inheritIO()
                .directory(targetDirectory)
                .start();
            if (classDataSharingSupport != null) {
                classDataSharingSupport.launched(process);
            }
        } finally {
            restartLock.unlock();
        }
    }

    private List<String> buildRunArguments() throws Exception {
        final List<String> reactorOutputs = mavenSession.getAllProjects().stream()
            .filter(this::isDependencyOfRunnableProject)
            .map(MavenProject::getBuild)
            .map(Build::getOutputDirectory)
            .toList();
        final String reactorClasses = String.join(File.pathSeparator, reactorOutputs);
        String classpathArgument = String.join(File.pathSeparator, reactorClasses, this.classpath);
        List<String> translatedJvmArguments = translateArguments(jvmArguments);

        var args = new ArrayList<String>();
        args.add(javaExecutable);
        addDebugArguments(args);
        addTestResourcesArguments(args);
        args.addAll(translatedJvmArguments);
        addUserProperties(args);
        addNativeImageAgentArguments(args, translatedJvmArguments);
        args.add("-classpath");
        int classpathIndex = args.size();
        args.add(classpathArgument);
        args.add("-XX:TieredStopAtLevel=1");
        int mainClassIndex = args.size();
        args.add(resolveMainClass());
        args.addAll(translateArguments(appArguments));
        if (classDataSharingSupport != null) {
            return classDataSharingSupport.prepareLaunch(args, classpathIndex, mainClassIndex, reactorOutputs, this.classpath);
        }
        return args;
    }

    private void addDebugArguments(List<String> args) {
        if (debug) {
            String suspend = debugSuspend ? "y" : "n";
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + suspend + ",address=" + debugHost + ":" + debugPort);
        }
    }

    private void addTestResourcesArguments(List<String> args) {
        if (testResourcesEnabled) {
            Path testResourcesSettingsDirectory = shared ? ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace) :
                AbstractTestResourcesMojo.serverSettingsDirectoryOf(targetDirectory.toPath());
            Optional<ServerSettings> serverSettings = ServerUtils.readServerSettings(testResourcesSettingsDirectory);
            serverSettings.ifPresent(settings -> testResourcesHelper.computeSystemProperties(settings)
                .forEach((k, v) -> args.add("-D" + k + "=" + v)));
        }
    }

    private void addNativeImageAgentArguments(List<String> args, List<String> translatedJvmArguments) throws MojoExecutionException {
        List<String> nativeImageAgentArguments = NativeImageAgentSupport.computeJvmArguments(mavenSession, runnableProject, targetDirectory, translatedJvmArguments);
        if (!nativeImageAgentArguments.isEmpty()) {
            if (watchForChanges) {
                getLog().warn("Native image agent metadata collection is intended for one-shot runs. Prefer mn:run -Dagent=true -Dmn.watch=false");
            }
            args.addAll(nativeImageAgentArguments);
        }
    }

    private void addUserProperties(List<String> args) {
        if (!mavenSession.getUserProperties().isEmpty()) {
            mavenSession.getUserProperties().forEach((k, v) -> args.add("-D" + k + "=" + v));
        }
    }

    private String resolveMainClass() {
        if (mainClass == null) {
            mainClass = runnableProject.getProperties().getProperty("exec.mainClass");
        }
        return mainClass;
    }

    private List<String> translateArguments(String arguments) throws Exception {
        if (arguments == null || arguments.isEmpty()) {
            return List.of();
        }
        return Arrays.asList(CommandLineUtils.translateCommandline(arguments));
    }

    private void runAotIfNeeded() {
        if (aotEnabled) {
            try {
                executorService.executeGoal(runnableProject, THIS_PLUGIN, AbstractAotAnalysisMojo.NAME, aotAnalysisConfiguration());
            } catch (MojoExecutionException e) {
                getLog().error(e.getMessage());
            }
        }
    }

    private Xpp3Dom aotAnalysisConfiguration() {
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        Xpp3Dom enabled = new Xpp3Dom("enabled");
        enabled.setValue(Boolean.TRUE.toString());
        configuration.addChild(enabled);
        return configuration;
    }

    private Xpp3Dom configurationValidationConfiguration() {
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        var plugin = runnableProject.getPlugin(THIS_PLUGIN);
        if (plugin == null) {
            return configuration;
        }
        Object pluginConfiguration = plugin.getConfiguration();
        if (!(pluginConfiguration instanceof Xpp3Dom pluginDom)) {
            return configuration;
        }
        Xpp3Dom validationConfiguration = pluginDom.getChild("configurationValidation");
        if (validationConfiguration != null) {
            configuration.addChild(new Xpp3Dom(validationConfiguration));
        }
        return configuration;
    }

    private void maybeStartTestResourcesServer() throws MojoExecutionException {
        testResourcesHelper.start();
    }

    private void maybeStopTestResourcesServer() throws MojoExecutionException {
        testResourcesHelper.stop(true);
    }

    private boolean compileProject() {
        // There can be multiple changes detected at the same time, so we want
        // to keep only one compilation request
        if (recompileRequested.get()) {
            return false;
        }
        recompileRequested.set(true);
        try {
            return doCompile();
        } finally {
            recompileRequested.set(false);
        }
    }

    private boolean doCompile() {
        Optional<Long> lastCompilationMillis = compilerService.compileProject();
        lastCompilationMillis.ifPresent(lc -> this.lastCompilation = lc);
        return lastCompilationMillis.isPresent();
    }

    private void killProcess() {
        Process current = process;
        if (current == null) {
            return;
        }
        boolean stopped = false;
        if (current.isAlive()) {
            if (getLog().isDebugEnabled()) {
                getLog().debug("Stopping the background process");
            }
            current.destroy();
            stopped = true;
            try {
                current.waitFor();
            } catch (InterruptedException e) {
                current.destroyForcibly();
                Thread.currentThread().interrupt();
            }
        }
        if (classDataSharingSupport != null) {
            classDataSharingSupport.launchEnded(current, stopped);
        }
    }

    private void stopOnShutdown() {
        if (classDataSharingSupport != null) {
            classDataSharingSupport.shutdown();
        }
        killProcess();
    }

    private static String normalize(Path path) {
        return path.toString().replace('\\', '/');
    }

    private static boolean pathMatches(String pattern, Path path) {
        return AbstractScanner.match(pattern, normalize(path));
    }

    private static boolean patternEquals(Path path, String includePattern, File directory) {
        try {
            var testPath = normalize(directory.toPath().resolve(includePattern).toAbsolutePath());
            return testPath.equals(normalize(path.toAbsolutePath()));
        } catch (InvalidPathException ex) {
            return false;
        }
    }

}
