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
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.*;
import org.apache.maven.shared.invoker.*;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Provides methods to compile a Maven project.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class CompilerService {

    public static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    public static final String MAVEN_JAR_PLUGIN = "org.apache.maven.plugins:maven-jar-plugin";
    public static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";
    public static final String GMAVEN_PLUS_PLUGIN = "org.codehaus.gmavenplus:gmavenplus-plugin";
    public static final String KOTLIN_MAVEN_PLUGIN = "org.jetbrains.kotlin:kotlin-maven-plugin";

    private static final String JAVA = "java";
    private static final String GROOVY = "groovy";
    private static final String KOTLIN = "kotlin";
    public static final String COMPILE_GOAL = "compile";
    public static final String RESOURCES_GOAL = "resources";

    private final Log log;
    private final Map<String, Path> sourceDirectories;
    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final ExecutorService executorService;
    private final ProjectDependenciesResolver resolver;
    private final Invoker invoker;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public CompilerService(MavenProject mavenProject, MavenSession mavenSession, ExecutorService executorService,
                           ProjectDependenciesResolver resolver) {
        this.resolver = resolver;
        this.log = new SystemStreamLog();
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.executorService = executorService;
        this.sourceDirectories = resolveSourceDirectories();
        this.invoker = new DefaultInvoker();
    }

    /**
     * Compiles the project.
     * @param copyResources whether to copy resources to the target directory.
     * @return the last compilation time millis.
     */
    public Optional<Long> compileProject(boolean copyResources) {
        Long lastCompilation = null;
        if (log.isDebugEnabled()) {
            log.debug("Compiling the project");
        }
        try {
            if (sourceDirectories.containsKey(GROOVY)) {
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "addSources");
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "generateStubs");
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, RESOURCES_GOAL);
                }
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, COMPILE_GOAL);
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, COMPILE_GOAL);
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "removeStubs");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(KOTLIN)) {
                executorService.executeGoal(KOTLIN_MAVEN_PLUGIN, "kapt");
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, RESOURCES_GOAL);
                }
                executorService.executeGoal(KOTLIN_MAVEN_PLUGIN, COMPILE_GOAL);
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, "compile#java-compile");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(JAVA)) {
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, RESOURCES_GOAL);
                }
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, COMPILE_GOAL);
                lastCompilation = System.currentTimeMillis();
            }
        } catch (MojoExecutionException e) {
            if (log.isErrorEnabled()) {
                log.error("Error while compiling the project: ", e);
            }
        }
        return Optional.ofNullable(lastCompilation);
    }

    /**
     * Resolves the source directories by checking the existence of Java, Groovy or Kotlin sources.
     */
    public Map<String, Path> resolveSourceDirectories() {
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

    /**
     * Resolves project dependencies for the given scopes.
     */
    public List<Dependency> resolveDependencies(String... scopes) {
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(scopes);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest dependencyResolutionRequest = new DefaultDependencyResolutionRequest(mavenProject, session);
            dependencyResolutionRequest.setResolutionFilter(filter);
            DependencyResolutionResult result = resolver.resolve(dependencyResolutionRequest);
            return result.getDependencies();
        } catch (org.apache.maven.project.DependencyResolutionException e) {
            if (log.isWarnEnabled()) {
                log.warn("Error while trying to resolve dependencies for the current project", e);
            }
            return Collections.emptyList();
        }
    }

    /**
     * Builds a classpath string for the given dependencies.
     */
    public String buildClasspath(List<Dependency> dependencies) {
        Comparator<Dependency> byGroupId = Comparator.comparing(d -> d.getArtifact().getGroupId());
        Comparator<Dependency> byArtifactId = Comparator.comparing(d -> d.getArtifact().getArtifactId());
        return dependencies.stream()
                .sorted(byGroupId.thenComparing(byArtifactId))
                .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
                .collect(Collectors.joining(File.pathSeparator));
    }

    /**
     * Packages the project by invoking the Jar plugin.
     */
    public InvocationResult packageProject() throws MavenInvocationException {
        InvocationRequest request = new DefaultInvocationRequest();
        request.setPomFile(mavenProject.getFile());
        request.setGoals(Collections.singletonList(MAVEN_JAR_PLUGIN + ":jar"));
        request.setBatchMode(true);
        request.setQuiet(true);
        return invoker.execute(request);
    }
}
