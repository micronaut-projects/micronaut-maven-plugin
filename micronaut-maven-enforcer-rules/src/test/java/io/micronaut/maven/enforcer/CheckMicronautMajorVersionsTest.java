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
package io.micronaut.maven.enforcer;

import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CheckMicronautMajorVersionsTest {

    @Test
    void passesWhenMicronautDependenciesUseSingleMajorVersion() {
        MavenProject project = projectWithDependencies(List.of(
            dependency("io.micronaut", "micronaut-inject", "5.0.0"),
            dependency("io.micronaut.serde", "micronaut-serde-processor", "5.1.0"),
            dependency("org.junit.jupiter", "junit-jupiter-api", "6.0.3")
        ));

        assertDoesNotThrow(() -> new CheckMicronautMajorVersions(project).execute());
    }

    @Test
    void failsWhenProjectDependenciesMixMicronautMajorVersions() {
        MavenProject project = projectWithDependencies(List.of(
            dependency("io.micronaut", "micronaut-inject", "5.0.0"),
            dependency("io.micronaut.validation", "micronaut-validation", "4.9.1")
        ));

        assertThrows(EnforcerRuleException.class, () -> new CheckMicronautMajorVersions(project).execute());
    }


    @Test
    void failsWhenMicronautVersionsAreProvidedViaProperties() {
        MavenProject project = projectWithDependencies(List.of(
            dependency("io.micronaut", "micronaut-inject", "${micronaut.version}"),
            dependency("io.micronaut.validation", "micronaut-validation", "${micronaut.validation.version}")
        ));
        project.getProperties().setProperty("micronaut.version", "5.0.0");
        project.getProperties().setProperty("micronaut.validation.version", "4.9.1");

        assertThrows(EnforcerRuleException.class, () -> new CheckMicronautMajorVersions(project).execute());
    }

    @Test
    void failsWhenDependencyManagementMixesMicronautMajorVersions() {
        MavenProject project = projectWithDependencies(List.of(
            dependency("io.micronaut", "micronaut-inject", "5.0.0")
        ));
        DependencyManagement dependencyManagement = new DependencyManagement();
        dependencyManagement.setDependencies(List.of(
            dependency("io.micronaut.platform", "micronaut-platform", "5.0.0"),
            dependency("io.micronaut", "micronaut-core-bom", "4.10.16")
        ));
        project.getModel().setDependencyManagement(dependencyManagement);

        assertThrows(EnforcerRuleException.class, () -> new CheckMicronautMajorVersions(project).execute());
    }

    private MavenProject projectWithDependencies(List<Dependency> dependencies) {
        MavenProject project = new MavenProject();
        project.setDependencies(dependencies);
        project.setBuild(new Build());
        return project;
    }

    private Build buildWithCompilerPlugin(Xpp3Dom configuration) {
        Plugin plugin = new Plugin();
        plugin.setGroupId("org.apache.maven.plugins");
        plugin.setArtifactId("maven-compiler-plugin");
        plugin.setConfiguration(configuration);

        Build build = new Build();
        build.setPlugins(List.of(plugin));
        return build;
    }

    private Xpp3Dom annotationProcessorPaths(Dependency... dependencies) {
        Xpp3Dom annotationProcessorPaths = new Xpp3Dom("annotationProcessorPaths");
        for (Dependency dependency : dependencies) {
            Xpp3Dom path = new Xpp3Dom("path");
            path.addChild(node("groupId", dependency.getGroupId()));
            path.addChild(node("artifactId", dependency.getArtifactId()));
            path.addChild(node("version", dependency.getVersion()));
            annotationProcessorPaths.addChild(path);
        }
        return annotationProcessorPaths;
    }

    private Xpp3Dom node(String name, String value) {
        Xpp3Dom node = new Xpp3Dom(name);
        node.setValue(value);
        return node;
    }

    private Dependency dependency(String groupId, String artifactId, String version) {
        Dependency dependency = new Dependency();
        dependency.setGroupId(groupId);
        dependency.setArtifactId(artifactId);
        dependency.setVersion(version);
        return dependency;
    }
}
