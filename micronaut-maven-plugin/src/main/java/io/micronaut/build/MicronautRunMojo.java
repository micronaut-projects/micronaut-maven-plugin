package io.micronaut.build;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
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
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.stream.Collectors;

import static java.nio.file.LinkOption.NOFOLLOW_LINKS;
import static java.nio.file.StandardWatchEventKinds.*;
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

    private static final String PROJECT_BUILD_DIRECTORY = "${project.build.directory}";
    private static final String PROJECT_SOURCE_DIRECTORY = "${project.build.sourceDirectory}";
    private final MavenSession mavenSession;
    private final BuildPluginManager pluginManager;
    private final ProjectDependenciesResolver resolver;
    private final ProjectBuilder projectBuilder;
    private MavenProject mavenProject;

    @Parameter(defaultValue = PROJECT_SOURCE_DIRECTORY)
    private File sourceDirectory;

    @Parameter(defaultValue = PROJECT_BUILD_DIRECTORY)
    private File targetDirectory;

    @Parameter(defaultValue = "${exec.mainClass}")
    private String mainClass;

    private WatchService watchService;
    private Map<WatchKey, Path> keys;
    private Process process;
    private Path projectRootDirectory;
    private List<Dependency> projectDependencies;
    private String classpath;

    @Inject
    public MicronautRunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager,
                            ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.pluginManager = pluginManager;
        this.resolver = resolver;
        this.projectBuilder = projectBuilder;
        this.projectRootDirectory = mavenProject.getBasedir().toPath();
        resolveDependencies();

    }

    @Override
    @SuppressWarnings("unchecked")
    public void execute() throws MojoExecutionException {
        boolean needsCompilation = mavenSession.getGoals().stream().noneMatch(PHASES_AFTER_COMPILE::contains);
        if (needsCompilation) {
            compileProject();
        }

        try {
            runApplication();
            Thread shutdownHook = new Thread(this::killProcess);
            Runtime.getRuntime().addShutdownHook(shutdownHook);

            this.watchService = FileSystems.getDefault().newWatchService();
            this.keys = new HashMap<>();

            Path sourcePath = sourceDirectory.toPath();
            registerAll(sourcePath);
            register(projectRootDirectory);

            for (;;) {
                WatchKey key;
                try {
                    key = watchService.take();
                } catch (InterruptedException x) {
                    return;
                }

                Path dir = keys.get(key);
                if (dir == null) {
                    getLog().warn("WatchKey not recognized: " + key.toString());
                    continue;
                }

                for (WatchEvent<?> event : key.pollEvents()) {
                    WatchEvent.Kind<?> kind = event.kind();
                    if (kind == OVERFLOW) {
                        continue;
                    }

                    WatchEvent<Path> ev = (WatchEvent<Path>) event;
                    Path name = ev.context();
                    Path child = dir.resolve(name);

                    needsCompilation = true;

                    if (dir.equals(projectRootDirectory)) {
                        if (child.endsWith("pom.xml")) {
                            getLog().info("Detected POM change. Resolving dependencies...");
                            rebuildMavenProject();
                            resolveDependencies();
                            needsCompilation = false;
                            getLog().info("Finished resolving dependencies. Recompilation is not necessary");
                        } else {
                            continue;
                        }
                    }

                    if (Files.isDirectory(child, NOFOLLOW_LINKS)) {
                        if (kind == ENTRY_CREATE) {
                            registerAll(child);
                        } else {
                            getLog().debug("Omitting changes in a directory");
                        }
                    } else {
                        if (needsCompilation) {
                            getLog().info("Detected change in " + child);
                            compileProject();
                            runApplication();
                        }
                    }
                }

                boolean valid = key.reset();
                if (!valid) {
                    keys.remove(key);
                    if (keys.isEmpty()) {
                        break;
                    }
                }
            }

        } catch (Exception e) {
            throw new MojoExecutionException("Exception while watching for changes", e);
        } finally {
            killProcess();
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

    private void registerAll(final Path start) throws IOException {
        Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                register(dir);
                return FileVisitResult.CONTINUE;
            }
        });

    }

    private void register(final Path dir) throws IOException {
        keys.put(dir.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE), dir);
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

    private void compileProject() {
        try {
            final Plugin compilerPlugin = mavenProject.getPlugin(MAVEN_COMPILER_PLUGIN);
            final Xpp3Dom compilerPluginConfiguration = (Xpp3Dom) compilerPlugin.getConfiguration();
            ExecutionEnvironment env = executionEnvironment(mavenProject, mavenSession, pluginManager);
            if (compilerPluginConfiguration != null) {
                executeMojo(compilerPlugin, goal("compile"), compilerPluginConfiguration, env);
            }
            final Plugin resourcesPlugin = mavenProject.getPlugin(MAVEN_RESOURCES_PLUGIN);
            executeMojo(resourcesPlugin, goal("resources"), configuration(), env);
        } catch (MojoExecutionException e) {
            getLog().error("Error while compiling the project: ", e);
        }
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
