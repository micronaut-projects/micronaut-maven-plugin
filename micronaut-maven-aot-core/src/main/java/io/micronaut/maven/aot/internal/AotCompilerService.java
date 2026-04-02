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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Project compilation helpers used by the extracted AOT mojos.
 */
@Singleton
public final class AotCompilerService {

    public static final String MAVEN_JAR_PLUGIN = "org.apache.maven.plugins:maven-jar-plugin";
    private static final String COMPILE_GOAL = "compile";

    private final Log log;
    private final MavenSession mavenSession;
    private final AotExecutorService executorService;
    private final ProjectDependenciesResolver resolver;

    @Inject
    public AotCompilerService(MavenSession mavenSession,
                              AotExecutorService executorService,
                              ProjectDependenciesResolver resolver) {
        this.mavenSession = mavenSession;
        this.executorService = executorService;
        this.resolver = resolver;
        this.log = new SystemStreamLog();
    }

    public Optional<Long> compileProject() {
        Long lastCompilation = null;
        try {
            MavenProject projectToCompile = mavenSession.getTopLevelProject();
            if (mavenSession.getAllProjects().contains(mavenSession.getCurrentProject().getParent())) {
                projectToCompile = mavenSession.getCurrentProject().getParent();
            }
            executorService.invokeGoals(projectToCompile, COMPILE_GOAL);
            lastCompilation = System.currentTimeMillis();
        } catch (Exception e) {
            if (log.isErrorEnabled()) {
                log.error("Error while compiling the project: ", e);
            }
        }
        return Optional.ofNullable(lastCompilation);
    }

    public List<Dependency> resolveDependencies(MavenProject project, String... scopes) {
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(scopes);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest request = new DefaultDependencyResolutionRequest(project, session);
            request.setResolutionFilter(filter);
            DependencyResolutionResult result = resolver.resolve(request);
            return result.getDependencies();
        } catch (org.apache.maven.project.DependencyResolutionException e) {
            if (log.isWarnEnabled()) {
                log.warn("Error while trying to resolve dependencies for the current project", e);
            }
            return Collections.emptyList();
        }
    }

    public String buildClasspath(List<Dependency> dependencies) {
        Comparator<Dependency> byGroupId = Comparator.comparing(d -> d.getArtifact().getGroupId());
        Comparator<Dependency> byArtifactId = Comparator.comparing(d -> d.getArtifact().getArtifactId());
        return dependencies.stream()
            .sorted(byGroupId.thenComparing(byArtifactId))
            .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
            .collect(Collectors.joining(File.pathSeparator));
    }

    public InvocationResult packageProject() throws MavenInvocationException {
        return executorService.invokeGoal(MAVEN_JAR_PLUGIN, "jar");
    }
}
