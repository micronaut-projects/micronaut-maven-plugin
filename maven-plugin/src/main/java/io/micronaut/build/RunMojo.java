package io.micronaut.build;

import io.methvin.watcher.DirectoryChangeEvent;
import io.methvin.watcher.DirectoryWatcher;
import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.FileSet;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.*;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.AbstractScanner;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.Os;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

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
@Mojo(name = "run", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class RunMojo extends AbstractMojo {

    public static final String MN_APP_ARGS = "mn.appArgs";
    public static final String EXEC_MAIN_CLASS = "${exec.mainClass}";

    private static final int LAST_COMPILATION_THRESHOLD = 500;
    private static final String JAVA = "java";
    private static final List<String> DEFAULT_EXCLUDES;

    static {
        DEFAULT_EXCLUDES = new ArrayList<>();
        Collections.addAll(DEFAULT_EXCLUDES, DirectoryScanner.DEFAULTEXCLUDES);
        Collections.addAll(DEFAULT_EXCLUDES, "**/.idea/**");
    }

    private final MavenSession mavenSession;
    private final ProjectDependenciesResolver resolver;
    private final ProjectBuilder projectBuilder;
    private final ToolchainManager toolchainManager;
    private final String javaExecutable;
    private final CompilerService compilerService;
    private final Path projectRootDirectory;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = EXEC_MAIN_CLASS, required = true)
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
     * List of inclusion/exclusion paths that should not trigger an application restart. Check the
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
    private List<String> jvmArguments;

    /**
     * List of additional arguments that will be passed to the application, after the class name.
     */
    @Parameter(property = MN_APP_ARGS)
    private List<String> appArguments;

    /**
     * Whether to watch for changes, or finish the execution after the first run.
     */
    @Parameter(property = "mn.watch", defaultValue = "true")
    private boolean watchForChanges;

    private MavenProject mavenProject;
    private DirectoryWatcher directoryWatcher;
    private Process process;
    private List<Dependency> projectDependencies;
    private String classpath;
    private int classpathHash;
    private long lastCompilation;
    private Map<String, Path> sourceDirectories;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public RunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager,
                   ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder, ToolchainManager toolchainManager,
                   CompilerService compilerService, ExecutorService executorService) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.resolver = resolver;
        this.projectBuilder = projectBuilder;
        this.projectRootDirectory = mavenProject.getBasedir().toPath();
        this.toolchainManager = toolchainManager;
        this.compilerService = compilerService;
        this.javaExecutable = findJavaExecutable();

        resolveDependencies();
        this.classpathHash = this.classpath.hashCode();
    }

    @Override
    @SuppressWarnings("unchecked")
    public void execute() throws MojoExecutionException {
        this.sourceDirectories = compilerService.resolveSourceDirectories();
        if (compilerService.needsCompilation()) {
            compilerService.compileProject(true);
        }

        try {
            runApplication();
            Thread shutdownHook = new Thread(this::killProcess);
            Runtime.getRuntime().addShutdownHook(shutdownHook);

            if (watchForChanges) {
                List<Path> pathsToWatch = new ArrayList<>(sourceDirectories.values());
                pathsToWatch.add(projectRootDirectory);

                if (watches != null && !watches.isEmpty()) {
                    for (FileSet fs : watches) {
                        File directory = new File(fs.getDirectory());
                        if (directory.exists()) {
                            pathsToWatch.add(directory.toPath());
                            //If neither includes nor excludes, add a default include
                            if ((fs.getIncludes() == null || fs.getIncludes().isEmpty()) && (fs.getExcludes() == null || fs.getExcludes().isEmpty())) {
                                fs.addInclude("**/*");
                            }
                        } else {
                            if (getLog().isWarnEnabled()) {
                                getLog().warn("The specified directory to watch doesn't exist: " + directory.getPath());
                            }
                        }
                    }
                }

                this.directoryWatcher = DirectoryWatcher
                        .builder()
                        .paths(pathsToWatch)
                        .listener(this::handleEvent)
                        .build();

                if (getLog().isDebugEnabled()) {
                    getLog().debug("Watching for changes...");
                }
                this.directoryWatcher.watch();
            } else if (process != null && process.isAlive()) {
                process.waitFor();
            }
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

    private void handleEvent(DirectoryChangeEvent event) throws IOException {
        Path path = event.path();
        Path parent = path.getParent();

        if (parent.equals(projectRootDirectory)) {
            if (path.endsWith("pom.xml") && rebuildMavenProject() && resolveDependencies() && classpathHasChanged()) {
                if (getLog().isInfoEnabled()) {
                    getLog().info("Detected POM dependencies change. Restarting application");
                }
                runApplication();
            }
        } else if (matches(path)) {
            if (getLog().isInfoEnabled()) {
                getLog().info("Detected change in " + projectRootDirectory.relativize(path).toString());
            }
            boolean compiledOk = compileProject();
            if (compiledOk) {
                runApplication();
            }
        }
    }

    private boolean matches(Path path) {
        // Apply default exclusions
        if (isDefaultExcluded(path) || isDirectory(path, NOFOLLOW_LINKS) || !isReadable(path) || hasBeenCompiledRecently()) {
            return false;
        }

        // Start by checking whether it's a change in any source directory
        boolean matches = this.sourceDirectories
                .values()
                .stream()
                .anyMatch(path.getParent()::startsWith);

        String relativePath = projectRootDirectory.relativize(path).toString();

        if (getLog().isDebugEnabled()) {
            String belongs = matches ? "belongs" : "does not belong";
            getLog().debug("Path [" + relativePath + "] " + belongs + " to a source directory");
        }

        if (watches != null && !watches.isEmpty()) {
            // Then process includes
            if (!matches) {
                for (FileSet fileSet : watches) {
                    if (fileSet.getIncludes() != null && !fileSet.getIncludes().isEmpty()) {
                        File directory = new File(fileSet.getDirectory());
                        if (directory.exists() && path.getParent().startsWith(directory.getAbsolutePath()))
                            for (String includePattern : fileSet.getIncludes()) {
                                if (DirectoryScanner.match(includePattern, path.toString()) || new File(directory, includePattern).toPath().toAbsolutePath().equals(path)) {
                                    matches = true;
                                    if (getLog().isDebugEnabled()) {
                                        getLog().debug("Path [" + relativePath + "] matched the include pattern [" + includePattern + "] of the directory [" + fileSet.getDirectory() + "]");
                                    }
                                    break;
                                }
                            }
                    }
                    if (matches) break;
                }
            }

            // Finally process excludes only if the path is matching
            if (matches) {
                for (FileSet fileSet : watches) {
                    if (fileSet.getExcludes() != null && !fileSet.getExcludes().isEmpty()) {
                        File directory = new File(fileSet.getDirectory());
                        if (directory.exists() && path.getParent().startsWith(directory.getAbsolutePath())) {
                            for (String excludePattern : fileSet.getExcludes()) {
                                if (DirectoryScanner.match(excludePattern, path.toString()) || new File(directory, excludePattern).toPath().toAbsolutePath().equals(path)) {
                                    matches = false;
                                    if (getLog().isDebugEnabled()) {
                                        getLog().debug("Path [" + relativePath + "] matched the exclude pattern [" + excludePattern + "] of the directory [" + fileSet.getDirectory() + "]");
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    if (!matches) break;
                }
            }
        }

        return matches;
    }

    private boolean isDefaultExcluded(Path path) {
        return path.startsWith(targetDirectory.getAbsolutePath()) ||
                DEFAULT_EXCLUDES.stream()
                        .anyMatch(excludePattern -> DirectoryScanner.match(excludePattern, path.toString()));
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
        } catch (Exception e) {
            // Do nothing
        }
    }

    private boolean rebuildMavenProject() {
        boolean success = true;
        try {
            ProjectBuildingRequest projectBuildingRequest = mavenSession.getProjectBuildingRequest();
            projectBuildingRequest.setResolveDependencies(true);
            ProjectBuildingResult build = projectBuilder.build(mavenProject.getArtifact(), projectBuildingRequest);
            MavenProject project = build.getProject();
            mavenProject = project;
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
        boolean success = true;
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(JavaScopes.COMPILE, JavaScopes.RUNTIME);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest dependencyResolutionRequest = new DefaultDependencyResolutionRequest(mavenProject, session);
            dependencyResolutionRequest.setResolutionFilter(filter);
            DependencyResolutionResult result = resolver.resolve(dependencyResolutionRequest);
            this.projectDependencies = result.getDependencies();
            buildClasspath();
        } catch (DependencyResolutionException e) {
            success = false;
            if (getLog().isWarnEnabled()) {
                getLog().warn("Error while trying to resolve dependencies for the current project", e);
            }
        }
        return success;
    }

    private void buildClasspath() {
        Comparator<Dependency> byGroupId = Comparator.comparing(d -> d.getArtifact().getGroupId());
        Comparator<Dependency> byArtifactId = Comparator.comparing(d -> d.getArtifact().getArtifactId());
        classpath = this.projectDependencies.stream()
                .sorted(byGroupId.thenComparing(byArtifactId))
                .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
                .collect(Collectors.joining(File.pathSeparator));
    }

    private boolean classpathHasChanged() {
        int oldClasspathHash = this.classpathHash;
        this.classpathHash = this.classpath.hashCode();
        return oldClasspathHash != classpathHash;

    }

    private void runApplication() throws IOException {
        String classpathArgument = new File(targetDirectory, "classes" + File.pathSeparator).getAbsolutePath() + this.classpath;
        List<String> args = new ArrayList<>();
        args.add(javaExecutable);

        if (debug) {
            String suspend = debugSuspend ? "y" : "n";
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + suspend + ",address=" + debugHost + ":" + debugPort);
        }

        if (jvmArguments != null && !jvmArguments.isEmpty()) {
            args.addAll(jvmArguments);
        }

        if (!mavenSession.getUserProperties().isEmpty()) {
            mavenSession.getUserProperties().forEach((k, v) -> args.add("-D" + k + "=" + v));
        }

        args.add("-classpath");
        args.add(classpathArgument);
        args.add("-XX:TieredStopAtLevel=1");
        args.add("-Dcom.sun.management.jmxremote");
        args.add(mainClass);

        if (appArguments != null && !appArguments.isEmpty()) {
            args.addAll(appArguments);
        }

        if (getLog().isDebugEnabled()) {
            getLog().debug("Running " + String.join(" ", args));
        }

        killProcess();
        process = new ProcessBuilder(args)
                .inheritIO()
                .directory(targetDirectory)
                .start();
    }

    private String findJavaExecutable() {
        String executable;
        Toolchain toolchain = this.toolchainManager.getToolchainFromBuildContext("jdk", mavenSession);
        if (toolchain != null) {
            executable = toolchain.findTool(JAVA);
        } else {
            File javaBinariesDir = new File(new File(System.getProperty("java.home")), "bin");
            if (Os.isFamily(Os.FAMILY_UNIX)) {
                executable = new File(javaBinariesDir, JAVA).getAbsolutePath();
            } else if (Os.isFamily(Os.FAMILY_WINDOWS)) {
                executable = new File(javaBinariesDir, "java.exe").getAbsolutePath();
            } else {
                executable = JAVA;
            }
        }
        return executable;
    }

    private boolean compileProject() {
        Optional<Long> lastCompilation = compilerService.compileProject(true);
        lastCompilation.ifPresent(lc -> this.lastCompilation = lc);
        return lastCompilation.isPresent();
    }

    private void killProcess() {
        if (getLog().isDebugEnabled()) {
            getLog().debug("Stopping the background process");
        }
        if (process != null && process.isAlive()) {
            process.destroy();
            try {
                process.waitFor();
            } catch (InterruptedException e) {
                process.destroyForcibly();
            }
        }
    }

}
