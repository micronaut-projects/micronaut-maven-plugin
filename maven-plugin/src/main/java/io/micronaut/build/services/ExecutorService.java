package io.micronaut.build.services;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

/**
 * Provides methods to execute goals on the current project
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class ExecutorService {

    private final MojoExecutor.ExecutionEnvironment executionEnvironment;
    private final MavenProject mavenProject;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public ExecutorService(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager) {
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
        this.mavenProject = mavenProject;
    }

    public void executeGoal(String pluginKey, String goal) throws MojoExecutionException {
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

    public void executeGoal(String pluginKey, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = mavenProject.getPlugin(pluginKey);
        if (plugin != null) {
            executeMojo(plugin, goal(goal), configuration, executionEnvironment);
        }
    }

    public void executeGoal(String pluginGroup, String pluginArtifact, String pluginVersion, String goal) throws MojoExecutionException {
        final Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration(), executionEnvironment);
    }

    public void executeGoal(String pluginGroup, String pluginArtifact, String pluginVersion, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment);
    }
}
