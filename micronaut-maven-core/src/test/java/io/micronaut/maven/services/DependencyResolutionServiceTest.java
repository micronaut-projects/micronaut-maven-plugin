package io.micronaut.maven.services;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResult;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DependencyResolutionServiceTest {

    @Test
    void artifactResultsForTreatsMissingDependencyManagementAsEmpty() throws Exception {
        RepositorySystem repositorySystem = mock(RepositorySystem.class);
        MavenSession session = mock(MavenSession.class);
        MavenProject project = mock(MavenProject.class);
        when(session.getRepositorySession()).thenReturn(new DefaultRepositorySystemSession());
        when(project.getRemoteProjectRepositories()).thenReturn(List.of(new RemoteRepository.Builder("central", "default", "https://repo.maven.apache.org/maven2").build()));
        when(repositorySystem.resolveDependencies(any(), any())).thenReturn(new DependencyResult(new DependencyRequest()));

        DependencyResolutionService service = new DependencyResolutionService(session, project, repositorySystem);
        service.artifactResultsFor(Stream.of(new DefaultArtifact("io.micronaut", "micronaut-runtime", "jar", "")), true);

        ArgumentCaptor<DependencyRequest> requestCaptor = ArgumentCaptor.forClass(DependencyRequest.class);
        verify(repositorySystem).resolveDependencies(any(), requestCaptor.capture());
        assertTrue(requestCaptor.getValue().getCollectRequest().getManagedDependencies().isEmpty());
        assertTrue(requestCaptor.getValue().getCollectRequest().getDependencies().isEmpty());
    }

    @Test
    void artifactResultsForUsesManagedVersionForVersionlessArtifacts() throws Exception {
        RepositorySystem repositorySystem = mock(RepositorySystem.class);
        MavenSession session = mock(MavenSession.class);
        MavenProject project = mock(MavenProject.class);
        DependencyManagement dependencyManagement = new DependencyManagement();
        org.apache.maven.model.Dependency managedDependency = new org.apache.maven.model.Dependency();
        managedDependency.setGroupId("io.micronaut");
        managedDependency.setArtifactId("micronaut-runtime");
        managedDependency.setType("jar");
        managedDependency.setVersion("4.0.0");
        dependencyManagement.addDependency(managedDependency);

        when(session.getRepositorySession()).thenReturn(new DefaultRepositorySystemSession());
        when(project.getRemoteProjectRepositories()).thenReturn(List.of());
        when(project.getDependencyManagement()).thenReturn(dependencyManagement);
        when(repositorySystem.resolveDependencies(any(), any())).thenReturn(new DependencyResult(new DependencyRequest()));

        DependencyResolutionService service = new DependencyResolutionService(session, project, repositorySystem);
        service.artifactResultsFor(Stream.of(new DefaultArtifact("io.micronaut", "micronaut-runtime", "jar", "")), true);

        ArgumentCaptor<DependencyRequest> requestCaptor = ArgumentCaptor.forClass(DependencyRequest.class);
        verify(repositorySystem).resolveDependencies(any(), requestCaptor.capture());
        List<Dependency> dependencies = requestCaptor.getValue().getCollectRequest().getDependencies();
        assertEquals(1, dependencies.size());
        assertEquals("4.0.0", dependencies.get(0).getArtifact().getVersion());
    }
}
