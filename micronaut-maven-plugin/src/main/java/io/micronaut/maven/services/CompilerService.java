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

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.DefaultDependencyResolutionRequest;
import org.apache.maven.project.DependencyResolutionRequest;
import org.apache.maven.project.DependencyResolutionResult;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectDependenciesResolver;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Provides methods to compile a Maven project.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class CompilerService {

    public static final String MAVEN_JAR_PLUGIN = "org.apache.maven.plugins:maven-jar-plugin";

    private static final String COMPILE_GOAL = "compile";
    private static final String JAVA = "java";
    private static final String GROOVY = "groovy";
    private static final String KOTLIN = "kotlin";

    private final Log log;
    private final MavenProject runnableProject;
    private final MavenSession mavenSession;
    private final ExecutorService executorService;
    private final ProjectDependenciesResolver resolver;

    @SuppressWarnings("MnInjectionPoints")
    @Inject
    public CompilerService(MavenSession mavenSession, ExecutorService executorService,
                           ProjectDependenciesResolver resolver) {
        this.mavenSession = mavenSession;
        this.runnableProject = findRunnableProject();
        this.resolver = resolver;
        this.log = new SystemStreamLog();
        this.executorService = executorService;
    }

    /**
     * Compiles the project.
     * @return the last compilation time millis.
     */
    public Optional<Long> compileProject() {
        Long lastCompilation = null;
        if (log.isDebugEnabled()) {
            log.debug("Compiling the project");
        }
        try {
            executorService.invokeGoals(COMPILE_GOAL);
            lastCompilation = System.currentTimeMillis();
        } catch (Exception e) {
            if (log.isErrorEnabled()) {
                log.error("Error while compiling the project: ", e);
            }
        }
        return Optional.ofNullable(lastCompilation);
    }

    /**
     * Resolves the source directories by checking the existence of Java, Groovy or Kotlin sources.
     *
     * @return a map with the language as key and the source directory as value.
     */
    public List<Path> resolveSourceDirectories() {
        if (log.isDebugEnabled()) {
            log.debug("Resolving source directories...");
        }
        return mavenSession.getProjects().stream()
                .map(this::resolveSourceDirectories)
                .flatMap(Set::stream)
                .toList();
    }

    private Set<Path> resolveSourceDirectories(MavenProject project) {
        Set<Path> sourceDirectoriesToResolve = new HashSet<>(3);
        for (String lang : Arrays.asList(JAVA, GROOVY, KOTLIN)) {
            Path sourceDir = project.getBasedir().toPath().resolve("src/main/" + lang);
            if (Files.exists(sourceDir)) {
                sourceDirectoriesToResolve.add(sourceDir);
            }
        }
        sourceDirectoriesToResolve.addAll(project.getCompileSourceRoots().stream()
                .map(File::new)
                .map(File::toPath)
                .filter(Files::exists)
                .collect(Collectors.toSet()));
        return sourceDirectoriesToResolve;
    }

    /**
     * Finds the Maven project that has the Micronaut Maven plugin defined.
     *
     * @return the Maven project
     */
    public MavenProject findRunnableProject() {
        return mavenSession.getProjects().stream()
                .filter(this::hasMicronautMavenPlugin)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("There are no projects with the Micronaut Maven plugin defined"));
    }

    private boolean hasMicronautMavenPlugin(MavenProject project) {
        return hasPlugin(project, "io.micronaut.maven:micronaut-maven-plugin");
    }

    private boolean hasPlugin(MavenProject project, String pluginKey) {
        String[] parts = pluginKey.split(":");
        String groupId = parts[0];
        String artifactId = parts[1];
        return project.getBuildPlugins().stream()
                .anyMatch(p -> p.getGroupId().equals(groupId) && p.getArtifactId().equals(artifactId));
    }

    /**
     * Resolves project dependencies for the given scopes.
     *
     * @param scopes the scopes to resolve dependencies for.
     * @return the list of dependencies.
     */
    public List<Dependency> resolveDependencies(String... scopes) {
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(scopes);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest dependencyResolutionRequest = new DefaultDependencyResolutionRequest(runnableProject, session);
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
     *
     * @param dependencies the dependencies to build the classpath for.
     * @return the classpath string.
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
     *
     * @return the invocation result.
     */
    public InvocationResult packageProject() throws MavenInvocationException {
        return executorService.invokeGoal(MAVEN_JAR_PLUGIN, "jar");
    }
}
