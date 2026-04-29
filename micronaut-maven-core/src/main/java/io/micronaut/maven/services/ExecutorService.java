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
package io.micronaut.maven.services;

import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.util.Arrays;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.executeMojo;
import static org.twdata.maven.mojoexecutor.MojoExecutor.executionEnvironment;
import static org.twdata.maven.mojoexecutor.MojoExecutor.goal;
import static org.twdata.maven.mojoexecutor.MojoExecutor.plugin;

/**
 * Provides methods to execute goals on the current project.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.0.0
 */
@Singleton
public class ExecutorService {

    private static final String TEST_RESOURCES_ENABLED_PROPERTY = "micronaut.test.resources.enabled";
    private static final Logger LOG = LoggerFactory.getLogger(ExecutorService.class);

    private final BuildPluginManager pluginManager;
    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final Invoker invoker;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public ExecutorService(MavenProject mavenProject,
                           MavenSession mavenSession,
                           BuildPluginManager pluginManager,
                           Invoker invoker) {
        this.pluginManager = pluginManager;
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.invoker = invoker;
    }

    /**
     * Executes the given goal from the given plugin coordinates.
     *
     * @param pluginKey The plugin coordinates in the format groupId:artifactId:version
     * @param goal The goal to execute
     * @throws MojoExecutionException If the goal execution fails
     */
    public void executeGoal(String pluginKey, String goal) throws MojoExecutionException {
        executeGoal(mavenProject, pluginKey, goal, null);
    }

    public final void executeGoal(MavenProject project, String pluginKey, String goal) throws MojoExecutionException {
        executeGoal(project, pluginKey, goal, null);
    }

    public final void executeGoal(MavenProject project, String pluginKey, String goal, Xpp3Dom overriddenConfiguration) throws MojoExecutionException {
        MavenProject targetProject = project == null ? mavenProject : project;
        Plugin plugin = resolveBuildPlugin(targetProject, pluginKey);
        if (plugin != null) {
            String goalName = goal;
            AtomicReference<String> executionId = new AtomicReference<>(goalName);
            if (goalName != null && goalName.indexOf('#') > -1) {
                int pos = goalName.indexOf('#');
                executionId.set(goal.substring(pos + 1));
                goalName = goalName.substring(0, pos);
            }
            Optional<PluginExecution> execution = plugin.getExecutions()
                .stream()
                .filter(e -> e.getId().equals(executionId.get()))
                .findFirst();
            Xpp3Dom configuration;
            if (overriddenConfiguration != null) {
                configuration = overriddenConfiguration;
            } else if (execution.isPresent()) {
                configuration = (Xpp3Dom) execution.get().getConfiguration();
            } else if (plugin.getConfiguration() != null) {
                configuration = (Xpp3Dom) plugin.getConfiguration();
            } else {
                configuration = configuration();
            }
            executeMojo(plugin, goal(goalName), configuration, executionEnvironment(targetProject, mavenSession, pluginManager));
        } else {
            throw new MojoExecutionException("Plugin not found: " + pluginKey);
        }
    }

    static Plugin resolveBuildPlugin(MavenProject targetProject, String pluginKey) {
        Plugin plugin = targetProject.getPlugin(pluginKey);
        if (plugin != null) {
            return plugin;
        }
        if (targetProject.getBuild() == null || targetProject.getBuild().getPluginManagement() == null) {
            return null;
        }
        return targetProject.getBuild().getPluginManagement().getPlugins()
            .stream()
            .filter(managedPlugin -> matchesPluginKey(managedPlugin, pluginKey))
            .findFirst()
            .orElse(null);
    }

    private static boolean matchesPluginKey(Plugin plugin, String pluginKey) {
        if (plugin == null) {
            return false;
        }
        if (pluginKey.equals(plugin.getKey())) {
            return true;
        }
        String artifactId = plugin.getArtifactId();
        if (artifactId == null) {
            return false;
        }
        String version = plugin.getVersion();
        return version != null && pluginKey.equals(plugin.getKey() + ":" + version);
    }

    /**
     * Executes a goal using the given arguments.
     *
     * @param pluginGroup plugin group id
     * @param pluginArtifact plugin artifact id
     * @param pluginVersion plugin version
     * @param goal goal to execute
     * @param configuration configuration for the goal
     * @throws MojoExecutionException if the goal execution fails
     */
    public void executeGoal(String pluginGroup, String pluginArtifact, String pluginVersion, String goal, Xpp3Dom configuration) throws MojoExecutionException {
        Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment(mavenProject, mavenSession, pluginManager));
    }

    /**
     * Executes a goal using the Maven shared invoker.
     *
     * @param pluginKey The plugin coordinates in the format groupId:artifactId
     * @param goal The goal to execute
     * @return The result of the invocation
     * @throws MavenInvocationException If the goal execution fails
     */
    public InvocationResult invokeGoal(String pluginKey, String goal) throws MavenInvocationException {
        return invokeGoals(pluginKey + ":" + goal);
    }

    /**
     * Executes goals using the Maven shared invoker.
     *
     * @param goals The goals to execute
     * @return The result of the invocation
     * @throws MavenInvocationException If the goal execution fails
     */
    public InvocationResult invokeGoals(String... goals) throws MavenInvocationException {
        return invokeGoals(mavenProject, goals);
    }

    /**
     * Executes goals using the Maven shared invoker.
     *
     * @param project The Maven project
     * @param goals The goals to execute
     * @return The result of the invocation
     * @throws MavenInvocationException If the goal execution fails
     */
    public InvocationResult invokeGoals(MavenProject project, String... goals) throws MavenInvocationException {
        DefaultInvocationRequest request = new DefaultInvocationRequest();
        request.setPomFile(resolveOriginalPom(project));
        File settingsFile = mavenSession.getRequest().getUserSettingsFile();
        if (settingsFile != null && settingsFile.exists()) {
            request.setUserSettingsFile(settingsFile);
        }
        Properties properties = new Properties();
        properties.put(TEST_RESOURCES_ENABLED_PROPERTY, "false");

        int loggingLevel = mavenSession.getRequest().getLoggingLevel();
        boolean quiet = loggingLevel >= MavenExecutionRequest.LOGGING_LEVEL_ERROR;

        request.setLocalRepositoryDirectory(new File(mavenSession.getLocalRepository().getBasedir()));
        request.addArgs(Arrays.asList(goals));
        request.setBatchMode(true);
        request.setQuiet(quiet);
        request.setAlsoMake(true);
        if (!quiet) {
            request.setErrorHandler(LOG::error);
            request.setOutputHandler(LOG::info);
        }
        request.setProperties(properties);
        return invoker.execute(request);
    }

    /**
     * Resolves the original pom.xml for a project. Plugins like the flatten-maven-plugin
     * may modify the project's POM file to point to a processed POM (e.g., in the target
     * directory). When the Maven invoker uses such a POM, {@code <module>} paths are resolved
     * relative to the POM file's location, which causes module resolution failures in
     * multi-module projects.
     *
     * @param project The Maven project
     * @return The original pom.xml file, or the project's file if the original cannot be found
     */
    static File resolveOriginalPom(MavenProject project) {
        File projectFile = project.getFile();
        if (projectFile == null || "pom.xml".equals(projectFile.getName())) {
            return projectFile;
        }
        String buildDirectory = project.getBuild() != null ? project.getBuild().getDirectory() : null;
        if (buildDirectory != null) {
            File buildDir = new File(buildDirectory);
            if (isInDirectory(projectFile, buildDir) || isKnownProcessedPom(projectFile)) {
                File projectDirectory = buildDir.getParentFile();
                if (projectDirectory != null) {
                    File originalPom = new File(projectDirectory, "pom.xml");
                    if (originalPom.isFile()) {
                        return originalPom;
                    }
                }
            }
        }
        return projectFile;
    }

    private static boolean isInDirectory(File file, File directory) {
        if (file == null || directory == null) {
            return false;
        }
        File current = file.getParentFile();
        while (current != null) {
            if (current.equals(directory)) {
                return true;
            }
            current = current.getParentFile();
        }
        return false;
    }

    private static boolean isKnownProcessedPom(File projectFile) {
        if (projectFile == null) {
            return false;
        }
        String name = projectFile.getName();
        return "flattened-pom.xml".equals(name)
            || ".flattened-pom.xml".equals(name)
            || "dependency-reduced-pom.xml".equals(name);
    }
}
