package io.micronaut.build;

import io.methvin.watcher.DirectoryChangeEvent;
import io.methvin.watcher.DirectoryWatcher;
import org.apache.maven.execution.MavenSession;
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
 * Mojo that handles mn:run goal.
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

    private final MavenSession mavenSession;
    private final ProjectDependenciesResolver resolver;
    private final ProjectBuilder projectBuilder;
    private final ExecutionEnvironment executionEnvironment;

    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    @Parameter(defaultValue = "${exec.mainClass}")
    private String mainClass;

    @Parameter(property = "mn.debug", defaultValue = "false")
    private boolean debug;

    @Parameter(property = "mn.debug.suspend", defaultValue = "false")
    private boolean debugSuspend;

    @Parameter(property = "mn.debug.port", defaultValue = "5005")
    private int debugPort;

    private MavenProject mavenProject;
    private DirectoryWatcher directoryWatcher;
    private Process process;
    private Path projectRootDirectory;
    private List<Dependency> projectDependencies;
    private String classpath;
    private long lastCompilation;
    private Map<String, Path> sourceDirectories;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public MicronautRunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager,
                            ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.resolver = resolver;
        this.projectBuilder = projectBuilder;
        this.projectRootDirectory = mavenProject.getBasedir().toPath();
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
        resolveDependencies();
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
            this.directoryWatcher = DirectoryWatcher
                    .builder()
                    .paths(pathsToWatch)
                    .listener(this::handleEvent)
                    .build();

            this.directoryWatcher.watch();
        } catch (Exception e) {
            throw new MojoExecutionException("Exception while watching for changes", e);
        } finally {
            killProcess();
            cleanup();
        }
    }

    private void resolveSourceDirectories() {
        AtomicReference<String> lang = new AtomicReference<>();
        this.sourceDirectories = Stream.of("java", "groovy", "kotlin")
                .peek(lang::set)
                .map(l -> new File(mavenProject.getBasedir(), "src/main/" + l))
                .filter(File::exists)
                .map(File::toPath)
                .collect(Collectors.toMap(path -> path.toString().substring(path.toString().lastIndexOf("/") + 1), Function.identity()));
        if (sourceDirectories.isEmpty()) {
            throw new IllegalStateException("Source folders not found for neither Java/Groovy/Kotlin");
        }
    }

    private void handleEvent(DirectoryChangeEvent event) throws IOException {
        Path path = event.path();
        Path parent = path.getParent();

        if (parent.equals(projectRootDirectory)) {
            if (path.endsWith("pom.xml")) {
                getLog().info("Detected POM change. Resolving dependencies...");
                rebuildMavenProject();
                resolveDependencies();
                getLog().info("Finished resolving dependencies. Recompilation is not necessary");
            }
        } else if (isChangeInSourceDirectory(parent, path)) {
            getLog().info("Detected change in " + path);
            boolean compiledOk = compileProject();
            if (compiledOk) {
                runApplication();
            }
        }
    }

    private boolean isChangeInSourceDirectory(Path parent, Path path) {
        return this.sourceDirectories
                .values()
                .stream()
                .anyMatch(parent::startsWith)
                    && !isDirectory(path, NOFOLLOW_LINKS)
                    && isReadable(path)
                    && !((System.currentTimeMillis() - lastCompilation) < LAST_COMPILATION_THRESHOLD);
    }

    private void cleanup() {
        try {
            directoryWatcher.close();
        } catch (IOException e) {
            // Do nothing
        }
    }

    private void rebuildMavenProject() {
        try {
            ProjectBuildingRequest projectBuildingRequest = mavenSession.getProjectBuildingRequest();
            projectBuildingRequest.setResolveDependencies(true);
            ProjectBuildingResult build = projectBuilder.build(mavenProject.getArtifact(), projectBuildingRequest);
            MavenProject project = build.getProject();
            mavenProject = project;
            mavenSession.setCurrentProject(project);
        } catch (ProjectBuildingException e) {
            getLog().warn("Error while trying to build the Maven project model", e);
        }
    }

    private void resolveDependencies() {
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(JavaScopes.COMPILE, JavaScopes.RUNTIME);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest dependencyResolutionRequest = new DefaultDependencyResolutionRequest(mavenProject, session);
            dependencyResolutionRequest.setResolutionFilter(filter);
            DependencyResolutionResult result = resolver.resolve(dependencyResolutionRequest);
            this.projectDependencies = result.getDependencies();
            buildClasspath();
        } catch (DependencyResolutionException e) {
            getLog().warn("Error while trying to resolve dependencies for the current project", e);
        }
    }

    private void buildClasspath() {
        classpath = this.projectDependencies.stream()
                .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
                .collect(Collectors.joining(File.pathSeparator));
    }

    private void runApplication() throws IOException {
        String classpathArgument = new File(targetDirectory, "classes:").getAbsolutePath() + this.classpath;
        List<String> args = new ArrayList<>();
        args.add("java");

        if (debug) {
            String suspend = debugSuspend ? "y" : "n";
            args.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=" + suspend + ",address=" + debugPort);
        }

        args.add("-classpath");
        args.add(classpathArgument);
        args.add("-noverify");
        args.add("-XX:TieredStopAtLevel=1");
        args.add("-Dcom.sun.management.jmxremote");
        args.add(mainClass);

        getLog().debug("Running " + String.join(" ", args));
        killProcess();
        process = new ProcessBuilder(args)
                .inheritIO()
                .directory(targetDirectory)
                .start();
    }

    private boolean compileProject() {
        try {
            if(sourceDirectories.containsKey("groovy")) {
                executeGoal(GMAVEN_PLUS_PLUGIN, "addSources");
                executeGoal(GMAVEN_PLUS_PLUGIN, "generateStubs");
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                executeGoal(GMAVEN_PLUS_PLUGIN, "compile");
                executeGoal(GMAVEN_PLUS_PLUGIN, "removeStubs");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey("kotlin")) {
                executeGoal(KOTLIN_MAVEN_PLUGIN, "kapt");
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(KOTLIN_MAVEN_PLUGIN, "compile");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile#java-compile");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey("java")) {
                executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                lastCompilation = System.currentTimeMillis();
            }
        } catch (MojoExecutionException e) {
            getLog().error("Error while compiling the project: ", e);
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
//            executeGoal(pluginKey, goal, configuration);
            executeMojo(plugin, goal(goal), configuration, executionEnvironment);
        }
    }

    private void executeGoal(String pluginKey, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = mavenProject.getPlugin(pluginKey);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment);
    }

    private void killProcess() {
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
