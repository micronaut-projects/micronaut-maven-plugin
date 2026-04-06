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

import io.micronaut.core.util.StringUtils;
import io.micronaut.testresources.buildtools.MavenDependency;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.DependencyManagement;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Stream;

/**
 * Utility methods for performing dependency resolution.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.0.0
 */
@Singleton
public class DependencyResolutionService {

    public static final String TEST_RESOURCES_GROUP = "io.micronaut.testresources";
    public static final String TEST_RESOURCES_ARTIFACT_ID_PREFIX = "micronaut-test-resources-";

    private static final String JAR_EXTENSION = "jar";

    private final MavenSession mavenSession;
    private final MavenProject mavenProject;
    private final RepositorySystem repositorySystem;

    @Inject
    public DependencyResolutionService(MavenSession mavenSession,
                                       MavenProject mavenProject,
                                       RepositorySystem repositorySystem) {
        this.mavenSession = mavenSession;
        this.mavenProject = mavenProject;
        this.repositorySystem = repositorySystem;
    }

    private static Stream<File> streamClasspath(List<ArtifactResult> resolutionResult) {
        return resolutionResult.stream()
            .map(ArtifactResult::getArtifact)
            .map(Artifact::getFile);
    }

    public static List<String> toClasspath(List<ArtifactResult> resolutionResult) {
        return streamClasspath(resolutionResult)
            .map(File::getAbsolutePath)
            .toList();
    }

    public static List<File> toClasspathFiles(List<ArtifactResult> resolutionResult) {
        return streamClasspath(resolutionResult).toList();
    }

    public static Dependency mavenDependencyToAetherDependency(org.apache.maven.model.Dependency dependency) {
        Artifact artifact = mavenDependencyToAetherArtifact(dependency);
        return new Dependency(artifact, dependency.getScope(), Boolean.valueOf(dependency.getOptional()));
    }

    public static Artifact mavenDependencyToAetherArtifact(org.apache.maven.model.Dependency dependency) {
        return new DefaultArtifact(
            dependency.getGroupId(),
            dependency.getArtifactId(),
            dependency.getClassifier(),
            dependency.getType(),
            dependency.getVersion()
        );
    }

    public static MavenDependency mavenDependencyToTestResourcesDependency(org.apache.maven.model.Dependency dependency) {
        return new MavenDependency(dependency.getGroupId(), dependency.getArtifactId(), dependency.getVersion());
    }

    public static Artifact testResourcesDependencyToAetherArtifact(MavenDependency dependency) {
        return new DefaultArtifact(dependency.getGroup(), dependency.getArtifact(), JAR_EXTENSION, dependency.getVersion());
    }

    /**
     * Performs a dependency request to compute the transitive dependencies of the given artifacts.
     *
     * @param artifacts The artifacts to resolve
     * @param applyManagedDependencies Whether to apply the managed dependencies of the project
     * @return The resolution result
     */
    public List<ArtifactResult> artifactResultsFor(Stream<Artifact> artifacts, boolean applyManagedDependencies) throws DependencyResolutionException {
        RepositorySystemSession repositorySession = mavenSession.getRepositorySession();
        DependencyFilter classpathFilter = DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME);
        CollectRequest collectRequest = new CollectRequest();
        collectRequest.setRepositories(mavenProject.getRemoteProjectRepositories());

        if (applyManagedDependencies) {
            DependencyManagement dependencyManagement = mavenProject.getDependencyManagement();
            List<org.apache.maven.model.Dependency> dependencies = dependencyManagement == null || dependencyManagement.getDependencies() == null
                ? List.of()
                : dependencyManagement.getDependencies();
            HashMap<String, Dependency> dependencyMap = new HashMap<>(dependencies.size());
            for (org.apache.maven.model.Dependency dependency : dependencies) {
                String ga = dependency.getGroupId() + ":" + dependency.getArtifactId();
                dependencyMap.putIfAbsent(ga, mavenDependencyToAetherDependency(dependency));
            }
            collectRequest.setManagedDependencies(new ArrayList<>(dependencyMap.values()));

            artifacts.forEach(artifact -> {
                if (StringUtils.isEmpty(artifact.getVersion())) {
                    dependencyMap.computeIfPresent(artifact.getGroupId() + ":" + artifact.getArtifactId(), (coordinates, dependency) -> {
                        collectRequest.addDependency(new Dependency(
                            new DefaultArtifact(artifact.getGroupId(), artifact.getArtifactId(), artifact.getExtension(), dependency.getArtifact().getVersion()),
                            JavaScopes.RUNTIME
                        ));
                        return dependency;
                    });
                } else {
                    collectRequest.addDependency(new Dependency(artifact, JavaScopes.RUNTIME));
                }
            });
        } else {
            artifacts.map(artifact -> new Dependency(artifact, JavaScopes.RUNTIME)).forEach(collectRequest::addDependency);
        }

        DependencyRequest dependencyRequest = new DependencyRequest(collectRequest, classpathFilter);
        DependencyResult dependencyResult = repositorySystem.resolveDependencies(repositorySession, dependencyRequest);
        return dependencyResult.getArtifactResults();
    }
}
