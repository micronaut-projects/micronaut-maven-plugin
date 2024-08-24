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
import io.micronaut.maven.aot.AotAnalysisMojo;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import io.micronaut.maven.testresources.AbstractTestResourcesMojo;
import io.micronaut.maven.testresources.TestResourcesHelper;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.FileSet;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
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
import org.codehaus.plexus.util.AbstractScanner;
import org.codehaus.plexus.util.cli.CommandLineUtils;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
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

import static io.micronaut.maven.MojoUtils.findJavaExecutable;
import static io.micronaut.maven.MojoUtils.hasMicronautMavenPlugin;
import static java.nio.file.Files.isDirectory;
import static java.nio.file.Files.isReadable;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;

/**
 * <p>Executes a Micronaut application in development mode.</p>
 *
 * <p>It watches for changes in the project tree. If there are changes in the {@code pom.xml} file, dependencies will be reloaded. If
 * the changes are anywhere underneath {@code src/main}, it will recompile the project and restart the application.</p>
 *
 * <p>The plugin can handle changes in all the languages supported by Micronaut: Java, Kotlin and Groovy.</p>
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
    public static final String THIS_PLUGIN = "io.micronaut.maven:micronaut-maven-plugin";

    private static final List<String> RELEVANT_SRC_DIRS = List.of("resources", "java", "kotlin", "groovy");
    private static final int LAST_COMPILATION_THRESHOLD = 500;
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

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public RunMojo(MavenSession mavenSession,
                   BuildPluginManager pluginManager,
                   ProjectBuilder projectBuilder,
                   ToolchainManager toolchainManager,
                   CompilerService compilerService,
                   ExecutorService executorService,
                   DependencyResolutionService dependencyResolutionService) {
        this.mavenSession = mavenSession;
        this.projectBuilder = projectBuilder;
        this.toolchainManager = toolchainManager;
        this.compilerService = compilerService;
        this.executorService = executorService;
        this.javaExecutable = findJavaExecutable(toolchainManager, mavenSession);
        this.dependencyResolutionService = dependencyResolutionService;
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
            Thread shutdownHook = new Thread(this::killProcess);
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
        }
        this.targetDirectory = new File(runnableProject.getBuild().getDirectory());
        this.testResourcesHelper = new TestResourcesHelper(testResourcesEnabled,
            shared,
            buildDirectory,
            explicitPort,
            clientTimeout,
            serverIdleTimeoutMinutes,
            runnableProject,
            mavenSession,
            dependencyResolutionService,
            toolchainManager,
            testResourcesVersion,
            classpathInference,
            testResourcesDependencies,
            sharedServerNamespace,
            debugServer);
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
            List<Dependency> dependencies = compilerService.resolveDependencies(runnableProject, JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME);
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
            runAotIfNeeded();
            String classpathArgument = new File(targetDirectory, "classes" + File.pathSeparator).getAbsolutePath() + this.classpath;

            var args = new ArrayList<String>();
            args.add(javaExecutable);

            if (debug) {
                String suspend = debugSuspend ? "y" : "n";
                args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + suspend + ",address=" + debugHost + ":" + debugPort);
            }

            if (testResourcesEnabled) {
                Path testResourcesSettingsDirectory = shared ? ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace) :
                    AbstractTestResourcesMojo.serverSettingsDirectoryOf(targetDirectory.toPath());
                Optional<ServerSettings> serverSettings = ServerUtils.readServerSettings(testResourcesSettingsDirectory);
                serverSettings.ifPresent(settings -> testResourcesHelper.computeSystemProperties(settings)
                    .forEach((k, v) -> args.add("-D" + k + "=" + v)));
            }

            if (jvmArguments != null && !jvmArguments.isEmpty()) {
                final String[] strings = CommandLineUtils.translateCommandline(jvmArguments);
                args.addAll(Arrays.asList(strings));
            }

            if (!mavenSession.getUserProperties().isEmpty()) {
                mavenSession.getUserProperties().forEach((k, v) -> args.add("-D" + k + "=" + v));
            }

            if (mainClass == null) {
                mainClass = runnableProject.getProperties().getProperty("exec.mainClass");
            }

            args.add("-classpath");
            args.add(classpathArgument);
            args.add("-XX:TieredStopAtLevel=1");
            args.add("-Dcom.sun.management.jmxremote");
            args.add(mainClass);

            if (appArguments != null && !appArguments.isEmpty()) {
                final String[] strings = CommandLineUtils.translateCommandline(appArguments);
                args.addAll(Arrays.asList(strings));
            }

            if (getLog().isDebugEnabled()) {
                getLog().debug("Running " + String.join(" ", args));
            }

            killProcess();
            process = new ProcessBuilder(args)
                .inheritIO()
                .directory(targetDirectory)
                .start();
        } finally {
            restartLock.unlock();
        }
    }

    private void runAotIfNeeded() {
        if (aotEnabled) {
            try {
                executorService.executeGoal(THIS_PLUGIN, AotAnalysisMojo.NAME);
            } catch (MojoExecutionException e) {
                getLog().error(e.getMessage());
            }
        }
    }

    private void maybeStartTestResourcesServer() throws MojoExecutionException {
        testResourcesHelper.start();
    }

    private void maybeStopTestResourcesServer() throws MojoExecutionException {
        testResourcesHelper.stop(false);
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
        if (process != null && process.isAlive()) {
            if (getLog().isDebugEnabled()) {
                getLog().debug("Stopping the background process");
            }
            process.destroy();
            try {
                process.waitFor();
            } catch (InterruptedException e) {
                process.destroyForcibly();
                Thread.currentThread().interrupt();
            }
        }
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
