/*
 * Copyright 2003-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.build.aot;

import io.micronaut.build.services.CompilerService;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.element;

/**
 * Base class for Micronaut AOT mojos.
 */
public abstract class AbstractMicronautAotCliMojo extends AbstractMicronautAotMojo {
    private final static String[] AOT_MODULES = new String[]{
            "api",
            "core",
            "cli",
            "std-optimizers"
    };

    @Parameter(property = "micronaut.aot.packageName", required = true)
    protected String packageName;


    @Inject
    public AbstractMicronautAotCliMojo(CompilerService compilerService) {
        super(compilerService);
    }

    protected abstract List<String> getExtraArgs() throws MojoExecutionException;

    @Override
    protected void doExecute() throws DependencyResolutionException, MojoExecutionException {
        if (compilerService.packageProject().isPresent()) {
            MojoExecutor.ExecutionEnvironment executionEnvironment = MojoExecutor.executionEnvironment(mavenProject, mavenSession, pluginManager);
            Plugin execPlugin = newExecPlugin("aot-exec");
            Xpp3Dom config = createExecPluginConfig();
            MojoExecutor.executeMojo(execPlugin, "exec", config, executionEnvironment);
        }
    }

    private Xpp3Dom createExecPluginConfig() throws DependencyResolutionException, MojoExecutionException {
        List<String> aotClasspath = resolveAotClasspath();
        List<String> applicationClasspath = resolveApplicationClasspath();
        List<String> classpath = new ArrayList<>(aotClasspath.size() + applicationClasspath.size());
        classpath.addAll(applicationClasspath);
        classpath.addAll(aotClasspath);
        MojoExecutor.Element[] runnerArgs = Stream.concat(Stream.of(
                        "-classpath",
                        String.join(File.pathSeparator, classpath),
                        "io.micronaut.aot.cli.Main",

                        // CLI args
                        "--optimizer-classpath=",
                        "--classpath=" + String.join(File.pathSeparator, classpath),
                        "--package=" + packageName,
                        "--runtime=" + runtime
                ), getExtraArgs().stream())
                .map(arg -> element("argument", arg))
                .toArray(MojoExecutor.Element[]::new);
        Xpp3Dom configuration = configuration(
                element("executable", "java"),
                element("arguments", runnerArgs)
        );
        return configuration;
    }

    protected Plugin newExecPlugin(String id) {
        Plugin execPlugin = new Plugin();
        execPlugin.setGroupId("org.codehaus.mojo");
        execPlugin.setArtifactId("exec-maven-plugin");
        execPlugin.setVersion("3.0.0");
        PluginExecution pluginExecution = new PluginExecution();
        pluginExecution.setGoals(Collections.singletonList("exec"));
        pluginExecution.setId(id);
        execPlugin.setExecutions(Collections.singletonList(pluginExecution));
        return execPlugin;
    }

    private List<String> resolveApplicationClasspath() throws DependencyResolutionException {
        return resolveClasspath(
                new DefaultArtifact(
                        mavenProject.getGroupId(),
                        mavenProject.getArtifactId(),
                        "jar",
                        mavenProject.getVersion()),
                Stream.of()
        );
    }

    private List<String> resolveAotClasspath() throws DependencyResolutionException {
        return resolveClasspath(null, Arrays.stream(AOT_MODULES).map(module -> new DefaultArtifact("io.micronaut.aot:micronaut-aot-" + module + ":" + micronautAotVersion)));
    }

    private List<String> resolveClasspath(Artifact rootArtifact, Stream<Artifact> artifacts) throws DependencyResolutionException {
        RepositorySystemSession repositorySession = mavenSession.getRepositorySession();
        DependencyFilter classpathFlter = DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME);

        CollectRequest collectRequest = new CollectRequest();
        if (rootArtifact != null) {
            collectRequest.setRoot(new Dependency(rootArtifact, JavaScopes.RUNTIME));
        }
        artifacts.map(a -> new Dependency(a, JavaScopes.RUNTIME))
                .forEach(collectRequest::addDependency);
        collectRequest.setRepositories(mavenProject.getRemoteProjectRepositories());

        DependencyRequest dependencyRequest = new DependencyRequest(collectRequest, classpathFlter);

        DependencyResult dependencyResult = repositorySystem.resolveDependencies(repositorySession, dependencyRequest);
        List<ArtifactResult> artifactResults = dependencyResult.getArtifactResults();

        return artifactResults.stream()
                .map(ArtifactResult::getArtifact)
                .map(Artifact::getFile)
                .map(File::getAbsolutePath)
                .collect(Collectors.toList());
    }
}
