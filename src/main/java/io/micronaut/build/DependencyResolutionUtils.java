package io.micronaut.build;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility methods for performing dependency resolution.
 */
public final class DependencyResolutionUtils {

    private DependencyResolutionUtils() {
    }

    public static List<String> toClasspath(List<ArtifactResult> resolutionResult) {
        return resolutionResult
                .stream()
                .map(ArtifactResult::getArtifact)
                .map(Artifact::getFile)
                .map(File::getAbsolutePath)
                .collect(Collectors.toList());
    }

    public static List<ArtifactResult> artifactResultsFor(MavenSession mavenSession,
                                                          MavenProject mavenProject,
                                                          RepositorySystem repositorySystem,
                                                          Stream<Artifact> artifacts) throws DependencyResolutionException {
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
}