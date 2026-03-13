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

import io.micronaut.maven.compat.MicronautMajorVersionChecker;
import org.apache.maven.enforcer.rule.api.AbstractEnforcerRule;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * Enforcer rule that checks that Micronaut dependencies participating in the build use the same major version.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.0.0
 */
@Named("checkMicronautMajorVersions")
public class CheckMicronautMajorVersions extends AbstractEnforcerRule {

    private static final String MAVEN_COMPILER_PLUGIN_ARTIFACT_ID = "maven-compiler-plugin";

    private final MavenProject project;
    private final MicronautMajorVersionChecker checker = new MicronautMajorVersionChecker();

    @Inject
    public CheckMicronautMajorVersions(MavenProject project) {
        this.project = project;
    }

    @Override
    public void execute() throws EnforcerRuleException {
        var coordinates = checker.collectEnforcerCoordinates(
            resolveVersions(project.getDependencies()),
            resolveVersions(project.getDependencyManagement() != null ? project.getDependencyManagement().getDependencies() : List.of()),
            resolveVersions(compilerAnnotationProcessorPaths())
        );

        if (checker.hasMixedMajors(coordinates)) {
            throw new EnforcerRuleException(checker.buildEnforcerFailureMessage(coordinates));
        }
    }

    private List<Dependency> compilerAnnotationProcessorPaths() {
        return Stream.concat(
                project.getBuildPlugins().stream(),
                project.getBuild() != null ? project.getBuild().getPlugins().stream() : Stream.empty()
            )
            .filter(plugin -> Objects.equals(plugin.getArtifactId(), MAVEN_COMPILER_PLUGIN_ARTIFACT_ID))
            .findFirst()
            .map(this::annotationProcessorPaths)
            .orElseGet(List::of);
    }

    private List<Dependency> annotationProcessorPaths(Plugin plugin) {
        Object configuration = plugin.getConfiguration();
        if (!(configuration instanceof Xpp3Dom dom)) {
            return List.of();
        }

        Xpp3Dom annotationProcessorPaths = dom.getChild("annotationProcessorPaths");
        if (annotationProcessorPaths == null) {
            return List.of();
        }

        List<Dependency> dependencies = new ArrayList<>();
        for (Xpp3Dom child : annotationProcessorPaths.getChildren()) {
            if (!"path".equals(child.getName()) && !"annotationProcessorPath".equals(child.getName())) {
                continue;
            }
            Dependency dependency = new Dependency();
            dependency.setGroupId(childValue(child, "groupId"));
            dependency.setArtifactId(childValue(child, "artifactId"));
            dependency.setVersion(childValue(child, "version"));
            dependencies.add(dependency);
        }
        return dependencies;
    }

    private String childValue(Xpp3Dom parent, String childName) {
        Xpp3Dom child = parent.getChild(childName);
        return child != null ? child.getValue() : null;
    }

    private List<Dependency> resolveVersions(List<Dependency> dependencies) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .map(dependency -> {
                Dependency resolved = new Dependency();
                resolved.setGroupId(dependency.getGroupId());
                resolved.setArtifactId(dependency.getArtifactId());
                resolved.setVersion(resolveVersion(dependency.getVersion()));
                return resolved;
            })
            .toList();
    }

    private String resolveVersion(String version) {
        if (version == null) {
            return null;
        }
        if (!version.startsWith("${") || !version.endsWith("}")) {
            return version;
        }
        String propertyName = version.substring(2, version.length() - 1);
        Properties properties = project.getProperties();
        String propertyValue = properties.getProperty(propertyName);
        if (propertyValue != null) {
            return propertyValue;
        }
        return project.getModel().getProperties().getProperty(propertyName, version);
    }

    @Override
    public String toString() {
        return "CheckMicronautMajorVersions";
    }
}
