package io.micronaut.build;

import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class PluginUtils {

    public static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    public static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";
    public static final String GMAVEN_PLUS_PLUGIN = "org.codehaus.gmavenplus:gmavenplus-plugin";
    public static final String KOTLIN_MAVEN_PLUGIN = "org.jetbrains.kotlin:kotlin-maven-plugin";

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
}
