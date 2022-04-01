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
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.RepositorySystem;
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

import static io.micronaut.build.aot.Constants.*;
import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.element;

/**
 * Base class for Micronaut AOT mojos.
 */
public abstract class AbstractMicronautAotCliMojo extends AbstractMicronautAotMojo {

    public static final String EXEC_MAVEN_PLUGIN_GROUP = "org.codehaus.mojo";
    public static final String EXEC_MAVEN_PLUGIN_ARTIFACT = "exec-maven-plugin";
    public static final String EXEC_MAVEN_PLUGIN_VERSION_PROPERTY = "exec-maven-plugin.version";
    public static final String DEFAULT_EXEC_MAVEN_PLUGIN_VERSION = "3.0.0";

    private static final String[] AOT_MODULES = new String[]{
            "api",
            "cli",
            "std-optimizers"
    };

    private final ExecutorService executorService;

    /**
     * Package name to use for generated sources.
     */
    @Parameter(property = MICRONAUT_AOT_PACKAGE_NAME)
    protected String packageName;

    @Parameter
    private List<org.apache.maven.model.Dependency> aotDependencies;

    @Inject
    public AbstractMicronautAotCliMojo(CompilerService compilerService, ExecutorService executorService,
                                       MavenProject mavenProject, MavenSession mavenSession, RepositorySystem repositorySystem) {
        super(compilerService, mavenProject, mavenSession, repositorySystem);
        this.executorService = executorService;
    }

    protected abstract List<String> getExtraArgs() throws MojoExecutionException;

    @Override
    protected void doExecute() throws MojoExecutionException, DependencyResolutionException {
        if (StringUtils.isEmpty(packageName)) {
            throw new MojoExecutionException(MICRONAUT_AOT_PACKAGE_NAME + " is not set, and is required if AOT is enabled");
        }
        try {
            getLog().info("Packaging project");
            compilerService.compileProject(true);
            InvocationResult packagingResult = compilerService.packageProject();
            if (packagingResult.getExitCode() != 0) {
                getLog().error("Error when packaging the project: ", packagingResult.getExecutionException());
            } else {
                executeAot();
            }
        } catch (MavenInvocationException e) {
            getLog().error("Error when packaging project", e);
        }
    }

    private void executeAot() throws DependencyResolutionException, MojoExecutionException {
        getLog().info("Executing Micronaut AOT analysis");
        Xpp3Dom config = createExecPluginConfig();

        executorService.executeGoal(
                EXEC_MAVEN_PLUGIN_GROUP,
                EXEC_MAVEN_PLUGIN_ARTIFACT,
                mavenProject.getProperties().getProperty(EXEC_MAVEN_PLUGIN_VERSION_PROPERTY, DEFAULT_EXEC_MAVEN_PLUGIN_VERSION),
                "exec",
                config
        );
    }

    private Xpp3Dom createExecPluginConfig() throws DependencyResolutionException, MojoExecutionException {
        List<String> aotClasspath = resolveAotClasspath();
        List<String> aotPluginsClasspath = resolveAotPluginsClasspath();
        List<String> applicationClasspath = resolveApplicationClasspath();

        List<String> classpath = new ArrayList<>(aotPluginsClasspath.size() + applicationClasspath.size());
        classpath.addAll(aotClasspath);
        classpath.addAll(aotPluginsClasspath);
        classpath.addAll(applicationClasspath);
        MojoExecutor.Element[] runnerArgs = Stream.concat(Stream.of(
                        "-classpath",
                        String.join(File.pathSeparator, aotClasspath),
                        MICRONAUT_AOT_MAIN_CLASS,

                        // CLI args
                        "--classpath=" + String.join(File.pathSeparator, classpath),
                        "--package=" + packageName,
                        "--runtime=" + runtime
                ), getExtraArgs().stream())
                .map(arg -> element("argument", arg))
                .toArray(MojoExecutor.Element[]::new);
        return configuration(
                element("executable", "java"),
                element("arguments", runnerArgs)
        );
    }

    private List<String> resolveApplicationClasspath() {
        String projectJar = new File(mavenProject.getBuild().getDirectory(), mavenProject.getBuild().getFinalName() + ".jar").getAbsolutePath();
        List<String> result = new ArrayList<>();
        result.add(projectJar);
        String classpath = compilerService.buildClasspath(compilerService.resolveDependencies(JavaScopes.RUNTIME));
        result.addAll(Arrays.asList(classpath.split(File.pathSeparator)));
        return result;
    }

    private List<String> resolveAotClasspath() throws DependencyResolutionException {
        Stream<Artifact> aotArtifacts = Arrays.stream(AOT_MODULES)
                .map(m -> new DefaultArtifact(MICRONAUT_AOT_GROUP_ID + ":" + MICRONAUT_AOT_ARTIFACT_ID_PREFIX + m + ":" + micronautAotVersion));
        return getClasspath(getArtifactResults(aotArtifacts));
    }

    private List<String> resolveAotPluginsClasspath() throws DependencyResolutionException {
        if (aotDependencies != null && !aotDependencies.isEmpty()) {
            Stream<Artifact> aotPlugins = aotDependencies.stream().map(d -> new DefaultArtifact(d.getGroupId(), d.getArtifactId(), d.getType(), d.getVersion()));
            return getClasspath(getArtifactResults(aotPlugins));
        } else {
            return Collections.emptyList();
        }
    }

    private List<ArtifactResult> getArtifactResults(Stream<Artifact> artifacts) throws DependencyResolutionException {
        RepositorySystemSession repositorySession = mavenSession.getRepositorySession();
        DependencyFilter classpathFilter = DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME);
        CollectRequest collectRequest = new CollectRequest();
        artifacts.map(a -> new Dependency(a, JavaScopes.RUNTIME))
                .forEach(collectRequest::addDependency);
        collectRequest.setRepositories(mavenProject.getRemoteProjectRepositories());

        DependencyRequest dependencyRequest = new DependencyRequest(collectRequest, classpathFilter);

        DependencyResult dependencyResult = repositorySystem.resolveDependencies(repositorySession, dependencyRequest);
        return dependencyResult.getArtifactResults();
    }

    private List<String> getClasspath(List<ArtifactResult> resolutionResult) {
        return resolutionResult
                .stream()
                .map(ArtifactResult::getArtifact)
                .map(Artifact::getFile)
                .map(File::getAbsolutePath)
                .collect(Collectors.toList());
    }
}
