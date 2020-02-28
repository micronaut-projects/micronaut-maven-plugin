package io.micronaut.build;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.*;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;

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

    private static final String MAVEN_DEPENDENCY_PLUGIN = "org.apache.maven.plugins:maven-dependency-plugin";
    private static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    private static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";

    private static final String OUTPUT_FILE_PARAMETER = "outputFile";
    private static final String CLASSPATH_FILE = "classpath.txt";
    private static final String PROJECT_BUILD_DIRECTORY = "${project.build.directory}";
    private static final String PROJECT_SOURCE_DIRECTORY = "${project.build.sourceDirectory}";

    private final MavenProject mavenProject;
    private final MavenSession mavenSession;

    @Parameter(defaultValue = PROJECT_SOURCE_DIRECTORY)
    private File sourceDirectory;

    @Parameter(defaultValue = PROJECT_BUILD_DIRECTORY)
    private File targetDirectory;

    @Parameter(defaultValue = "${exec.mainClass}")
    private String mainClass;

    private ExecutionEnvironment env;
    private WatchService watchService;
    private Map<WatchKey, Path> keys;
    private Process process;

    @Inject
    public MicronautRunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.env = executionEnvironment(mavenProject, mavenSession, pluginManager);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void execute() throws MojoExecutionException {
        boolean isCompiled = mavenSession.getGoals().stream().anyMatch(PHASES_AFTER_COMPILE::contains);
        if (!isCompiled) {
            compileProject();
        }

        try {
            runApplication();

            this.watchService = FileSystems.getDefault().newWatchService();
            this.keys = new HashMap<>();

            Path sourcePath = sourceDirectory.toPath();
            registerAll(sourcePath);

            getLog().info("Watching for changes in " + sourcePath.toString());

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

                for (WatchEvent<?> event: key.pollEvents()) {
                    compileProject();
                    runApplication();

                    WatchEvent.Kind<?> kind = event.kind();
                    if (kind == OVERFLOW) {
                        continue;
                    }

                    WatchEvent<Path> ev = (WatchEvent<Path>) event;
                    Path name = ev.context();
                    Path child = dir.resolve(name);

                    if (kind == ENTRY_CREATE && Files.isDirectory(child, NOFOLLOW_LINKS)) {
                        registerAll(child);
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
            throw new MojoExecutionException("Exception while creating a watch service", e);
        }
    }

    private void registerAll(final Path start) throws IOException {
        Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                keys.put(dir.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE), dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private void buildClasspath() throws MojoExecutionException {
        final Plugin dependenciesPlugin = mavenProject.getPlugin(MAVEN_DEPENDENCY_PLUGIN);
        final Xpp3Dom configuration = configuration(element(name(OUTPUT_FILE_PARAMETER), PROJECT_BUILD_DIRECTORY + "/" + CLASSPATH_FILE));
        executeMojo(dependenciesPlugin, goal("build-classpath"), configuration, env);
    }

    private void runApplication() throws MojoExecutionException, IOException, InterruptedException {
        File classpathFile = new File(targetDirectory, CLASSPATH_FILE);
        if (!classpathFile.exists()) {
            buildClasspath();
        }
        String classpath = new File(targetDirectory, "classes:").getAbsolutePath();
        classpath += new String(Files.readAllBytes(classpathFile.toPath()));
        List<String> args = new ArrayList<>();
        args.add("java");
        args.add("-classpath");
        args.add(classpath);
        args.add("-noverify");
        args.add("-XX:TieredStopAtLevel=1");
        args.add("-Dcom.sun.management.jmxremote");
        args.add(mainClass);

        getLog().debug("Running " + String.join(" ", args));
        if (process != null && process.isAlive()) {
            process.destroy();
            process.waitFor();
        }
        process = new ProcessBuilder(args)
                .inheritIO()
                .directory(targetDirectory)
                .start();
    }

    private void compileProject() throws MojoExecutionException {
        final Plugin compilerPlugin = mavenProject.getPlugin(MAVEN_COMPILER_PLUGIN);
        final Xpp3Dom compilerPluginConfiguration = (Xpp3Dom) compilerPlugin.getConfiguration();
        if (compilerPluginConfiguration != null) {
            executeMojo(compilerPlugin, goal("compile"), compilerPluginConfiguration, env);
            buildClasspath();
        }
        final Plugin resourcesPlugin = mavenProject.getPlugin(MAVEN_RESOURCES_PLUGIN);
        executeMojo(resourcesPlugin, goal("resources"), configuration(), env);
    }

}
