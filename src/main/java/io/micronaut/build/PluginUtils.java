package io.micronaut.build;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.jkube.kit.common.AssemblyConfiguration;
import org.eclipse.jkube.kit.config.image.build.BuildConfiguration;
import org.eclipse.jkube.kit.config.image.build.DockerFileBuilder;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class PluginUtils {

    private static final String JAVA = "java";
    private static final String GROOVY = "groovy";
    private static final String KOTLIN = "kotlin";

    public static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    public static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";
    public static final String GMAVEN_PLUS_PLUGIN = "org.codehaus.gmavenplus:gmavenplus-plugin";
    public static final String KOTLIN_MAVEN_PLUGIN = "org.jetbrains.kotlin:kotlin-maven-plugin";

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

    public static void executeGoal(MojoExecutor.ExecutionEnvironment executionEnvironment, MavenProject mavenProject, String pluginKey, String goal) throws MojoExecutionException {
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

    public static void executeGoal(MojoExecutor.ExecutionEnvironment executionEnvironment, MavenProject mavenProject, String pluginKey, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = mavenProject.getPlugin(pluginKey);
        if (plugin != null) {
            executeMojo(plugin, goal(goal), configuration, executionEnvironment);
        }
    }

    public static void executeGoal(MojoExecutor.ExecutionEnvironment executionEnvironment, MavenProject mavenProject, String pluginGroup, String pluginArtifact, String pluginVersion, String goal) throws MojoExecutionException {
        final Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration(), executionEnvironment);
    }

    public static void executeGoal(MojoExecutor.ExecutionEnvironment executionEnvironment, MavenProject mavenProject, String pluginGroup, String pluginArtifact, String pluginVersion, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment);
    }

    /*
     * Source: https://github.com/eclipse/jkube/blob/v1.0.0/jkube-kit/build/api/src/main/java/org/eclipse/jkube/kit/build/api/assembly/AssemblyManager.java#L247
     */
    @SuppressWarnings("deprecation")
    public static DockerFileBuilder createDockerFileBuilder(BuildConfiguration buildConfig, AssemblyConfiguration assemblyConfig) {
        DockerFileBuilder builder =
                new DockerFileBuilder()
                        .env(buildConfig.getEnv())
                        .labels(buildConfig.getLabels())
                        .expose(buildConfig.getPorts())
                        .run(buildConfig.getRunCmds())
                        .volumes(buildConfig.getVolumes())
                        .user(buildConfig.getUser());
        if (buildConfig.getMaintainer() != null) {
            builder.maintainer(buildConfig.getMaintainer());
        }
        if (buildConfig.getWorkdir() != null) {
            builder.workdir(buildConfig.getWorkdir());
        }
        if (assemblyConfig != null) {
            builder.add(assemblyConfig.getTargetDir(), "")
                    .basedir(assemblyConfig.getTargetDir())
                    .assemblyUser(assemblyConfig.getUser())
                    .exportTargetDir(assemblyConfig.getExportTargetDir());
        } else {
            builder.exportTargetDir(false);
        }

        builder.baseImage(buildConfig.getFrom());

        if (buildConfig.getHealthCheck() != null) {
            builder.healthCheck(buildConfig.getHealthCheck());
        }

        if (buildConfig.getCmd() != null){
            builder.cmd(buildConfig.getCmd());
        }

        if (buildConfig.getEntryPoint() != null){
            builder.entryPoint(buildConfig.getEntryPoint());
        }

        if (buildConfig.optimise()) {
            builder.optimise();
        }

        return builder;
    }

    public static Optional<Long> compileProject(Log log, Map<String, Path> sourceDirectories, ExecutionEnvironment executionEnvironment, MavenProject mavenProject, boolean copyResources) {
        Long lastCompilation = null;
        if (log.isDebugEnabled()) {
            log.debug("Compiling the project");
        }
        try {
            if(sourceDirectories.containsKey(GROOVY)) {
                executeGoal(executionEnvironment, mavenProject, GMAVEN_PLUS_PLUGIN, "addSources");
                executeGoal(executionEnvironment, mavenProject, GMAVEN_PLUS_PLUGIN, "generateStubs");
                if (copyResources) {
                    executeGoal(executionEnvironment, mavenProject, MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executeGoal(executionEnvironment, mavenProject, MAVEN_COMPILER_PLUGIN, "compile");
                executeGoal(executionEnvironment, mavenProject, GMAVEN_PLUS_PLUGIN, "compile");
                executeGoal(executionEnvironment, mavenProject, GMAVEN_PLUS_PLUGIN, "removeStubs");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(KOTLIN)) {
                executeGoal(executionEnvironment, mavenProject, KOTLIN_MAVEN_PLUGIN, "kapt");
                if (copyResources) {
                    executeGoal(executionEnvironment, mavenProject, MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executeGoal(executionEnvironment, mavenProject, KOTLIN_MAVEN_PLUGIN, "compile");
                executeGoal(executionEnvironment, mavenProject, MAVEN_COMPILER_PLUGIN, "compile#java-compile");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(JAVA)) {
                if (copyResources) {
                    executeGoal(executionEnvironment, mavenProject, MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executeGoal(executionEnvironment, mavenProject, MAVEN_COMPILER_PLUGIN, "compile");
                lastCompilation = System.currentTimeMillis();
            }
        } catch (MojoExecutionException e) {
            if (log.isErrorEnabled()) {
                log.error("Error while compiling the project: ", e);
            }
        }
        return Optional.ofNullable(lastCompilation);
    }

    public static boolean needsCompilation(MavenSession mavenSession) {
        return mavenSession.getGoals().stream().noneMatch(PHASES_AFTER_COMPILE::contains);
    }

    public static Map<String, Path> resolveSourceDirectories(Log log, MavenProject mavenProject) {
        if (log.isDebugEnabled()) {
            log.debug("Resolving source directories...");
        }
        AtomicReference<String> lang = new AtomicReference<>();
        Map<String, Path> sourceDirectories = Stream.of(JAVA, GROOVY, KOTLIN)
                .peek(lang::set)
                .map(l -> new File(mavenProject.getBasedir(), "src/main/" + l))
                .filter(File::exists)
                .peek(f -> {
                    if (log.isDebugEnabled()) {
                        log.debug("Found source: " + f.getPath());
                    }
                })
                .map(File::toPath)
                .collect(Collectors.toMap(path -> lang.get(), Function.identity()));
        if (sourceDirectories.isEmpty()) {
            throw new IllegalStateException("Source folders not found for neither Java/Groovy/Kotlin");
        }
        return sourceDirectories;
    }
}
