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
package io.micronaut.maven.aot.internal;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.util.Arrays;
import java.util.Properties;

import static org.twdata.maven.mojoexecutor.MojoExecutor.executeMojo;
import static org.twdata.maven.mojoexecutor.MojoExecutor.executionEnvironment;
import static org.twdata.maven.mojoexecutor.MojoExecutor.goal;
import static org.twdata.maven.mojoexecutor.MojoExecutor.plugin;

/**
 * Minimal goal execution support used by the extracted AOT mojos.
 */
@Singleton
public final class AotExecutorService {

    private static final String TEST_RESOURCES_ENABLED_PROPERTY = "micronaut.test.resources.enabled";

    private final BuildPluginManager pluginManager;
    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final Invoker invoker;

    @Inject
    public AotExecutorService(MavenProject mavenProject,
                              MavenSession mavenSession,
                              BuildPluginManager pluginManager,
                              Invoker invoker) {
        this.pluginManager = pluginManager;
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.invoker = invoker;
    }

    public void executeGoal(String pluginGroup,
                            String pluginArtifact,
                            String pluginVersion,
                            String goal,
                            Xpp3Dom configuration) throws MojoExecutionException {
        Plugin plugin = plugin(pluginGroup, pluginArtifact, pluginVersion);
        executeMojo(plugin, goal(goal), configuration, executionEnvironment(mavenProject, mavenSession, pluginManager));
    }

    public InvocationResult invokeGoal(String pluginKey, String goal) throws MavenInvocationException {
        return invokeGoals(pluginKey + ":" + goal);
    }

    public InvocationResult invokeGoals(String... goals) throws MavenInvocationException {
        return invokeGoals(mavenProject, goals);
    }

    public InvocationResult invokeGoals(MavenProject project, String... goals) throws MavenInvocationException {
        DefaultInvocationRequest request = new DefaultInvocationRequest();
        request.setPomFile(resolveOriginalPom(project));

        File settingsFile = mavenSession.getRequest().getUserSettingsFile();
        if (settingsFile != null && settingsFile.exists()) {
            request.setUserSettingsFile(settingsFile);
        }

        Properties properties = new Properties();
        properties.put(TEST_RESOURCES_ENABLED_PROPERTY, "false");

        request.setLocalRepositoryDirectory(new File(mavenSession.getLocalRepository().getBasedir()));
        request.addArgs(Arrays.asList(goals));
        request.setBatchMode(true);
        request.setQuiet(true);
        request.setAlsoMake(true);
        request.setErrorHandler(System.err::println);
        request.setOutputHandler(System.out::println);
        request.setProperties(properties);
        return invoker.execute(request);
    }

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
