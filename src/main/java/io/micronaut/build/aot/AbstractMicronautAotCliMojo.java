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
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.element;

/**
 * Base class for Micronaut AOT mojos.
 */
public abstract class AbstractMicronautAotCliMojo extends AbstractMicronautAotMojo {

    public static final String EXEC_MAVEN_PLUGIN_GROUP = "org.codehaus.mojo";
    public static final String EXEC_MAVEN_PLUGIN_ARTIFACT = "exec-maven-plugin";
    public static final String EXEC_MAVEN_PLUGIN_VERSION = "3.0.0";

    private static final String DEFAULT_PACKAGE = "com.example";

    private static final String[] AOT_MODULES = new String[]{
            "api",
            "core",
            "std-optimizers"
    };

    private final ExecutorService executorService;

    /**
     * Package name to use for generated sources.
     */
    @Parameter(property = "micronaut.aot.packageName", required = true, defaultValue = DEFAULT_PACKAGE)
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
    protected void doExecute() throws MojoExecutionException {
        if (DEFAULT_PACKAGE.equals(packageName)) {
            getLog().warn("Micronaut AOT will generate sources in the " + DEFAULT_PACKAGE + " package");
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
        } catch (MojoExecutionException | DependencyResolutionException e) {
            getLog().error("Error when running Micronaut AOT", e);
        }
    }

    private void executeAot() throws DependencyResolutionException, MojoExecutionException {
        getLog().info("Executing Micronaut AOT analysis");
        Xpp3Dom config = createExecPluginConfig();

        executorService.executeGoal(
                EXEC_MAVEN_PLUGIN_GROUP,
                EXEC_MAVEN_PLUGIN_ARTIFACT,
                EXEC_MAVEN_PLUGIN_VERSION,
                "exec",
                config
        );
    }

    private Xpp3Dom createExecPluginConfig() throws DependencyResolutionException, MojoExecutionException {
        List<String> aotCliClasspath = resolveAotCliClasspath();
        List<String> aotPluginsClasspath = resolveAotPluginsClasspath();
        List<String> aotModulesClasspath = resolveAotModulesClasspath();
        List<String> applicationClasspath = resolveApplicationClasspath();

        List<String> classpath = new ArrayList<>(aotPluginsClasspath.size() + + aotModulesClasspath.size() + applicationClasspath.size());
        classpath.addAll(aotCliClasspath);
        classpath.addAll(aotPluginsClasspath);
        classpath.addAll(aotModulesClasspath);
        classpath.addAll(applicationClasspath);
        MojoExecutor.Element[] runnerArgs = Stream.concat(Stream.of(
                        "-classpath",
                        String.join(File.pathSeparator, classpath),
                        "io.micronaut.aot.cli.Main",

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

    private List<String> resolveAotModulesClasspath() throws DependencyResolutionException {
        Stream<Artifact> aotArtifacts = Arrays.stream(AOT_MODULES)
                .map(m -> new DefaultArtifact("io.micronaut.aot:micronaut-aot-" + m + ":" + micronautAotVersion));
        return getClasspath(getArtifactResults(aotArtifacts));
    }

    private List<String> resolveAotPluginsClasspath() throws DependencyResolutionException {
        Stream<Artifact> aotPlugins = aotDependencies.stream().map(d -> new DefaultArtifact(d.getGroupId(), d.getArtifactId(), d.getType(), d.getVersion()));
        return getClasspath(getArtifactResults(aotPlugins));
    }

    private List<String> resolveAotCliClasspath() throws DependencyResolutionException {
        Stream<Artifact> aotCliArtifact = Stream.of(new DefaultArtifact("io.micronaut.aot:micronaut-aot-cli:" + micronautAotVersion));
        return getClasspath(getArtifactResults(aotCliArtifact));
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
