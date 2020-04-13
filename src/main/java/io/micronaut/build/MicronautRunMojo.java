package io.micronaut.build;

import io.methvin.watcher.DirectoryChangeEvent;
import io.methvin.watcher.DirectoryWatcher;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.FileSet;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
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
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.Os;
import org.codehaus.plexus.util.xml.Xpp3Dom;
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
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.nio.file.Files.isDirectory;
import static java.nio.file.Files.isReadable;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;
import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

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
@Mojo(name = "run", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class MicronautRunMojo extends AbstractMojo {

    /**
     * @see <a href="https://maven.apache.org/ref/3.6.3/maven-core/lifecycles.html#default_Lifecycle">default Lifecycle</a>
     */
    private static final List<String> PHASES_AFTER_COMPILE = Arrays.asList(
            "compile",
            "process-classes",
            "generate-test-sources",
            "process-test-sources",
            "generate-test-resources",
            "process-test-resources",
            "test-compile",
            "process-test-classes",
            "test",
            "prepare-package",
            "package",
            "pre-integration-test",
            "integration-test",
            "post-integration-test",
            "verify",
            "install",
            "deploy");

    private static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    private static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";
    private static final String GMAVEN_PLUS_PLUGIN = "org.codehaus.gmavenplus:gmavenplus-plugin";
    private static final String KOTLIN_MAVEN_PLUGIN = "org.jetbrains.kotlin:kotlin-maven-plugin";
    private static final int LAST_COMPILATION_THRESHOLD = 500;
    private static final String JAVA = "java";
    private static final String GROOVY = "groovy";
    private static final String KOTLIN = "kotlin";
    private static final List<String> DEFAULT_EXCLUDES;

    static {
        DEFAULT_EXCLUDES = new ArrayList<>();
        Collections.addAll(DEFAULT_EXCLUDES, DirectoryScanner.DEFAULTEXCLUDES);
        Collections.addAll(DEFAULT_EXCLUDES, "**/.idea/**");
    }

    private final MavenSession mavenSession;
    private final ProjectDependenciesResolver resolver;
    private final ProjectBuilder projectBuilder;
    private final ExecutionEnvironment executionEnvironment;
    private final ToolchainManager toolchainManager;
    private final String javaExecutable;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}")
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
     * List of inclusion/exclusion paths that should not trigger an application restart. Check the
     * <a href="https://maven.apache.org/ref/3.3.9/maven-model/apidocs/org/apache/maven/model/FileSet.html">FileSet</a>
     * documentation for more details.
     *
     * @see <a href="https://maven.apache.org/ref/3.3.9/maven-model/apidocs/org/apache/maven/model/FileSet.html">FileSet</a>
     */
    @Parameter
    private List<FileSet> watches;

    /**
     * List of additional arguments that will be passed to the JVM process.
     */
    @Parameter
    private List<String> arguments;

    private MavenProject mavenProject;
    private DirectoryWatcher directoryWatcher;
    private Process process;
    private Path projectRootDirectory;
    private List<Dependency> projectDependencies;
    private String classpath;
    private int classpathHash;
    private long lastCompilation;
    private Map<String, Path> sourceDirectories;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public MicronautRunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager,
                            ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder, ToolchainManager toolchainManager) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.resolver = resolver;
        this.projectBuilder = projectBuilder;
        this.projectRootDirectory = mavenProject.getBasedir().toPath();
        this.toolchainManager = toolchainManager;
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
        this.javaExecutable = findJavaExecutable();
        resolveDependencies();
        this.classpathHash = this.classpath.hashCode();
    }

    @Override
    @SuppressWarnings("unchecked")
    public void execute() throws MojoExecutionException {
        resolveSourceDirectories();
        boolean needsCompilation = mavenSession.getGoals().stream().noneMatch(PHASES_AFTER_COMPILE::contains);
        if (needsCompilation) {
            compileProject();
        }

        try {
            runApplication();
            Thread shutdownHook = new Thread(this::killProcess);
            Runtime.getRuntime().addShutdownHook(shutdownHook);

            List<Path> pathsToWatch = new ArrayList<>(sourceDirectories.values());
            pathsToWatch.add(projectRootDirectory);

            if (watches != null && !watches.isEmpty()) {
                for (FileSet fs : watches) {
                    File directory = new File(fs.getDirectory());
                    if (directory.exists()) {
                        pathsToWatch.add(directory.toPath());
                        //If neither includes nor excludes, add a default include
                        if ((fs.getIncludes() == null || fs.getIncludes().isEmpty() ) && (fs.getExcludes() == null || fs.getExcludes().isEmpty() )) {
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

    private void resolveSourceDirectories() {
        if (getLog().isDebugEnabled()) {
            getLog().debug("Resolving source directories...");
        }
        AtomicReference<String> lang = new AtomicReference<>();
        this.sourceDirectories = Stream.of(JAVA, GROOVY, KOTLIN)
                .peek(lang::set)
                .map(l -> new File(mavenProject.getBasedir(), "src/main/" + l))
                .filter(File::exists)
                .peek(f -> { if (getLog().isDebugEnabled()) { getLog().debug("Found source: " + f.getPath()); } })
                .map(File::toPath)
                .collect(Collectors.toMap(path -> lang.get(), Function.identity()));
        if (sourceDirectories.isEmpty()) {
            throw new IllegalStateException("Source folders not found for neither Java/Groovy/Kotlin");
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
            if (!matches){
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
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + suspend + ",address=" + debugPort);
        }

        if (arguments != null && arguments.size() > 0) {
            args.addAll(arguments);
        }

        args.add("-classpath");
        args.add(classpathArgument);
        args.add("-XX:TieredStopAtLevel=1");
        args.add("-Dcom.sun.management.jmxremote");
        args.add(mainClass);

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
        Toolchain toolchain = this.toolchainManager.getToolchainFromBuildContext("jdk", mavenSession);
        if (toolchain != null) {
            return toolchain.findTool(JAVA);
        } else {
            File javaBinariesDir = new File(new File(System.getProperty("java.home")), "bin");
            if (Os.isFamily(Os.FAMILY_UNIX)) {
                return new File(javaBinariesDir, JAVA).getAbsolutePath();
            } else if (Os.isFamily(Os.FAMILY_WINDOWS)) {
                return new File(javaBinariesDir, "java.exe").getAbsolutePath();
            } else {
                return JAVA;
            }
        }
    }

    private boolean compileProject() {
        if (getLog().isDebugEnabled()) {
            getLog().debug("Compiling the project");
        }
        try {
            if(sourceDirectories.containsKey(GROOVY)) {
                executeGoal(GMAVEN_PLUS_PLUGIN, "addSources");
                executeGoal(GMAVEN_PLUS_PLUGIN, "generateStubs");
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                executeGoal(GMAVEN_PLUS_PLUGIN, "compile");
                executeGoal(GMAVEN_PLUS_PLUGIN, "removeStubs");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(KOTLIN)) {
                executeGoal(KOTLIN_MAVEN_PLUGIN, "kapt");
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(KOTLIN_MAVEN_PLUGIN, "compile");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile#java-compile");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(JAVA)) {
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                lastCompilation = System.currentTimeMillis();
            }
        } catch (MojoExecutionException e) {
            if (getLog().isErrorEnabled()) {
                getLog().error("Error while compiling the project: ", e);
            }
            return false;
        }
        return true;
    }

    private void executeGoal(String pluginKey, String goal) throws MojoExecutionException {
        final Plugin plugin = mavenProject.getPlugin(pluginKey);
        if (plugin != null) {
            AtomicReference<String> executionId = new AtomicReference<>(goal);
            if (goal != null && goal.length() > 0 && goal.indexOf('#') > -1) {
                int pos = goal.indexOf('#');
                executionId.set(goal.substring(pos + 1));
                goal = goal.substring(0, pos);
            }
            Optional<PluginExecution> execution = plugin
                    .getExecutions()
                    .stream()
                    .filter(e -> e.getId().equals(executionId.get()))
                    .findFirst();
            Xpp3Dom configuration;
            if (execution.isPresent()) {
                configuration = (Xpp3Dom) execution.get().getConfiguration();
            } else if (plugin.getConfiguration() != null) {
                configuration = (Xpp3Dom) plugin.getConfiguration();
            } else {
                configuration = configuration();
            }
            executeMojo(plugin, goal(goal), configuration, executionEnvironment);
        }
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
