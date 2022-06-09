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
import org.apache.maven.project.MavenProject;
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

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility methods for performing dependency resolution.
 */
@Singleton
public class DependencyResolutionService {

    private final MavenSession mavenSession;

    private final MavenProject mavenProject;

    private final RepositorySystem repositorySystem;

    @Inject
    public DependencyResolutionService(MavenSession mavenSession, MavenProject mavenProject, RepositorySystem repositorySystem) {
        this.mavenSession = mavenSession;
        this.mavenProject = mavenProject;
        this.repositorySystem = repositorySystem;
    }

    public static List<String> toClasspath(List<ArtifactResult> resolutionResult) {
        return resolutionResult
                .stream()
                .map(ArtifactResult::getArtifact)
                .map(Artifact::getFile)
                .map(File::getAbsolutePath)
                .collect(Collectors.toList());
    }

    /**
     * Performs a dependency request to compute the transitive dependencies of the given artifacts.
     */
    public List<ArtifactResult> artifactResultsFor(Stream<Artifact> artifacts) throws DependencyResolutionException {
        RepositorySystemSession repositorySession = mavenSession.getRepositorySession();
        DependencyFilter classpathFilter = DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME);
        CollectRequest collectRequest = new CollectRequest();
        artifacts.map(a -> new Dependency(a, JavaScopes.RUNTIME))
                .forEach(collectRequest::addDependency);
        collectRequest.setRepositories(mavenProject.getRemoteProjectRepositories());
        List<Dependency> managedDependencies = mavenProject.getDependencyManagement().getDependencies().stream()
                .map(d -> {
                    Artifact artifact = new DefaultArtifact(d.getGroupId(), d.getArtifactId(), d.getClassifier(), "jar", d.getVersion());
                    return new Dependency(artifact, d.getScope(), Boolean.valueOf(d.getOptional()));
                })
                .collect(Collectors.toList());
        collectRequest.setManagedDependencies(managedDependencies);

        DependencyRequest dependencyRequest = new DependencyRequest(collectRequest, classpathFilter);

        DependencyResult dependencyResult = repositorySystem.resolveDependencies(repositorySession, dependencyRequest);
        return dependencyResult.getArtifactResults();
    }
}
