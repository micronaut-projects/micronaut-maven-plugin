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
package io.micronaut.maven.aot;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.maven.aot.Constants.MICRONAUT_AOT_ARTIFACT_ID_PREFIX;
import static io.micronaut.maven.aot.Constants.MICRONAUT_AOT_GROUP_ID;
import static io.micronaut.maven.aot.Constants.MICRONAUT_AOT_MAIN_CLASS;
import static io.micronaut.maven.aot.Constants.MICRONAUT_AOT_PACKAGE_NAME;
import static io.micronaut.maven.services.DependencyResolutionService.toClasspath;
import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.element;

/**
 * Base class for Micronaut AOT mojos.
 */
public abstract class AbstractMicronautAotCliMojo extends AbstractMicronautAotMojo {

    public static final String EXEC_MAVEN_PLUGIN_GROUP = "org.codehaus.mojo";
    public static final String EXEC_MAVEN_PLUGIN_ARTIFACT = "exec-maven-plugin";
    public static final String EXEC_MAVEN_PLUGIN_VERSION_PROPERTY = "exec-maven-plugin.version";
    public static final String DEFAULT_EXEC_MAVEN_PLUGIN_VERSION = "3.1.0";

    private static final String[] AOT_MODULES = {
        "api",
        "cli",
        "std-optimizers"
    };

    /**
     * Package name to use for generated sources.
     */
    @Parameter(property = MICRONAUT_AOT_PACKAGE_NAME)
    protected String packageName;

    private final ExecutorService executorService;

    private final DependencyResolutionService dependencyResolutionService;

    @Parameter
    private List<org.apache.maven.model.Dependency> aotDependencies;

    /**
     * Additional JVM arguments to pass to the AOT compiler (eg: <code>--enable-preview</code>).
     *
     * @since 4.0.2
     */
    @Parameter(property = "micronaut.aot.jvmArgs")
    private List<String> aotJvmArgs;

    @Inject
    public AbstractMicronautAotCliMojo(CompilerService compilerService, ExecutorService executorService,
                                       MavenProject mavenProject, DependencyResolutionService dependencyResolutionService) {
        super(compilerService, mavenProject);
        this.executorService = executorService;
        this.dependencyResolutionService = dependencyResolutionService;
    }

    protected abstract List<String> getExtraArgs() throws MojoExecutionException;

    @Override
    protected void doExecute() throws MojoExecutionException, DependencyResolutionException {
        if (StringUtils.isEmpty(packageName)) {
            throw new MojoExecutionException(MICRONAUT_AOT_PACKAGE_NAME + " is not set, and is required if AOT is enabled");
        }
        try {
            getLog().info("Packaging project");
            compilerService.compileProject();
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

        try {
            executorService.executeGoal(
                EXEC_MAVEN_PLUGIN_GROUP,
                EXEC_MAVEN_PLUGIN_ARTIFACT,
                mavenProject.getProperties().getProperty(EXEC_MAVEN_PLUGIN_VERSION_PROPERTY, DEFAULT_EXEC_MAVEN_PLUGIN_VERSION),
                "exec",
                config
            );
        } catch (MojoExecutionException e) {
            getLog().error("Error when executing Micronaut AOT: " + e.getMessage());
            String commandLine = Arrays.stream(config.getChild("arguments").getChildren())
                .map(Xpp3Dom::getValue)
                .collect(Collectors.joining(" "));
            getLog().error("Command line was: java " + commandLine);
            throw e;
        }

    }

    private Xpp3Dom createExecPluginConfig() throws DependencyResolutionException, MojoExecutionException {
        List<String> aotClasspath = resolveAotClasspath();
        List<String> aotPluginsClasspath = resolveAotPluginsClasspath();
        List<String> applicationClasspath = resolveApplicationClasspath();

        var classpath = new ArrayList<String>(aotPluginsClasspath.size() + applicationClasspath.size());
        classpath.addAll(aotClasspath);
        classpath.addAll(aotPluginsClasspath);
        classpath.addAll(applicationClasspath);
        Stream<String> jvmArgs = Optional.ofNullable(aotJvmArgs).orElse(List.of()).stream();
        Stream<String> mainArgs = Stream.of(
            "-classpath",
            String.join(File.pathSeparator, aotClasspath),
            MICRONAUT_AOT_MAIN_CLASS,

            // CLI args
            "--classpath=" + String.join(File.pathSeparator, classpath),
            "--package=" + packageName,
            "--runtime=" + runtime
        );
        MojoExecutor.Element[] runnerArgs = Stream.concat(Stream.concat(jvmArgs, mainArgs), getExtraArgs().stream())
            .map(arg -> element("argument", arg))
            .toArray(MojoExecutor.Element[]::new);
        return configuration(
            element("executable", "java"),
            element("arguments", runnerArgs)
        );
    }

    private List<String> resolveApplicationClasspath() {
        String projectJar = new File(mavenProject.getBuild().getDirectory(), mavenProject.getBuild().getFinalName() + ".jar")
            .getAbsolutePath();
        var result = new ArrayList<String>();
        result.add(projectJar);
        String classpath = compilerService.buildClasspath(compilerService.resolveDependencies(mavenProject, JavaScopes.RUNTIME));
        result.addAll(Arrays.asList(classpath.split(File.pathSeparator)));
        return result;
    }

    private List<String> resolveAotClasspath() throws DependencyResolutionException {
        Stream<Artifact> aotArtifacts = Arrays.stream(AOT_MODULES)
            .map(m -> new DefaultArtifact(MICRONAUT_AOT_GROUP_ID + ":" + MICRONAUT_AOT_ARTIFACT_ID_PREFIX + m + ":" + micronautAotVersion));
        return toClasspath(dependencyResolutionService.artifactResultsFor(aotArtifacts, false));
    }

    private List<String> resolveAotPluginsClasspath() throws DependencyResolutionException {
        if (aotDependencies != null && !aotDependencies.isEmpty()) {
            Stream<Artifact> aotPlugins = aotDependencies.stream()
                .map(d -> new DefaultArtifact(d.getGroupId(), d.getArtifactId(), d.getType(), d.getVersion()));
            return toClasspath(dependencyResolutionService.artifactResultsFor(aotPlugins, false));
        } else {
            return Collections.emptyList();
        }
    }

}
