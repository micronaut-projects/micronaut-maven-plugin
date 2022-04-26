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
 * Provides methods to execute goals on the current project.
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

    /**
     * Executes the given goal from the given plugin coordinates.
     */
    public void executeGoal(String pluginKey, String goal) throws MojoExecutionException {
        final Plugin plugin = mavenProject.getPlugin(pluginKey);
        if (plugin != null) {
            AtomicReference<String> executionId = new AtomicReference<>(goal);
            if (goal != null && goal.indexOf('#') > -1) {
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
        } else {
            throw new MojoExecutionException("Plugin not found: " + pluginKey);
        }
    }

    /**
     * Executes a goal using the given arguments.
     */
    public void executeGoal(String pluginGroup, String pluginArtifact, String pluginVersion, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        final Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment);
    }
}
