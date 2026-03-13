/*
 * Copyright 2017-2023 original authors
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
package io.micronaut.maven;

import org.apache.maven.MavenExecutionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Parent;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class MicronautMajorVersionLifecycleExtensionTest {

    @Test
    void passesWhenMicronautCoordinatesUseSingleMajorVersion() throws MavenExecutionException {
        MavenProject project = projectWithDependencies(List.of(
            dependency("io.micronaut", "micronaut-inject", "5.0.0"),
            dependency("io.micronaut.serde", "micronaut-serde-processor", "5.1.0")
        ));
        Parent parent = new Parent();
        parent.setGroupId("io.micronaut.platform");
        parent.setArtifactId("micronaut-parent");
        parent.setVersion("5.0.0-SNAPSHOT");
        project.getModel().setParent(parent);

        MavenSession session = mock(MavenSession.class);
        when(session.getAllProjects()).thenReturn(List.of(project));

        assertDoesNotThrow(() -> new MicronautMajorVersionLifecycleExtension().afterProjectsRead(session));
    }

    @Test
    void failsWhenParentAndDependencyManagementMixMicronautMajorVersions() {
        MavenProject project = projectWithDependencies(List.of());
        project.setArtifactId("foo-infra");

        Parent parent = new Parent();
        parent.setGroupId("io.micronaut.platform");
        parent.setArtifactId("micronaut-parent");
        parent.setVersion("5.0.0-SNAPSHOT");
        project.getModel().setParent(parent);

        DependencyManagement dependencyManagement = new DependencyManagement();
        dependencyManagement.setDependencies(List.of(
            dependency("io.micronaut", "micronaut-core-bom", "4.10.16")
        ));
        project.getModel().setDependencyManagement(dependencyManagement);

        MavenSession session = mock(MavenSession.class);
        when(session.getAllProjects()).thenReturn(List.of(project));

        assertThrows(MavenExecutionException.class, () -> new MicronautMajorVersionLifecycleExtension().afterProjectsRead(session));
    }

    private MavenProject projectWithDependencies(List<Dependency> dependencies) {
        MavenProject project = new MavenProject();
        project.setDependencies(dependencies);
        return project;
    }

    private Dependency dependency(String groupId, String artifactId, String version) {
        Dependency dependency = new Dependency();
        dependency.setGroupId(groupId);
        dependency.setArtifactId(artifactId);
        dependency.setVersion(version);
        return dependency;
    }
}
