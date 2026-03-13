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

import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.MavenExecutionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Parent;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.annotations.Component;

import java.util.List;
import java.util.Comparator;
import java.util.Locale;
import java.util.Objects;
import java.util.OptionalInt;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Lifecycle extension that fails fast when Micronaut major versions are mixed in project models.
 */
@Component(role = AbstractMavenLifecycleParticipant.class, hint = "micronaut-major-version-check")
public final class MicronautMajorVersionLifecycleExtension extends AbstractMavenLifecycleParticipant {

    private static final String MAVEN_COMPILER_PLUGIN_ARTIFACT_ID = "maven-compiler-plugin";
    private static final Pattern LEADING_MAJOR_PATTERN = Pattern.compile("^(\\d+)");

    @Override
    public void afterProjectsRead(MavenSession session) throws MavenExecutionException {
        for (MavenProject project : session.getAllProjects()) {
            List<MicronautCoordinate> coordinates = collectMicronautCoordinates(project);
            OptionalInt baselineMajor = coordinates.stream()
                .filter(coordinate -> "parent".equals(coordinate.source()) || "dependencymanagement".equals(coordinate.source()))
                .map(MicronautCoordinate::majorVersion)
                .filter(OptionalInt::isPresent)
                .mapToInt(OptionalInt::getAsInt)
                .findFirst();

            if (baselineMajor.isEmpty()) {
                continue;
            }

            List<MicronautCoordinate> conflicts = coordinates.stream()
                .filter(coordinate -> coordinate.majorVersion().isPresent())
                .filter(coordinate -> coordinate.majorVersion().getAsInt() != baselineMajor.getAsInt())
                .toList();

            if (!conflicts.isEmpty()) {
                throw new MavenExecutionException(buildFailureMessage(project, baselineMajor.getAsInt(), coordinates, conflicts), project.getFile());
            }
        }
    }

    private List<MicronautCoordinate> collectMicronautCoordinates(MavenProject project) {
        return Stream.of(
                micronautParent(project.getModel().getParent()),
                directMicronautDependencies(project.getDependencies(), "dependency"),
                directMicronautDependencies(dependencyManagementDependencies(project), "dependencyManagement")
            )
            .flatMap(List::stream)
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .distinct()
            .toList();
    }

    private List<Dependency> dependencyManagementDependencies(MavenProject project) {
        DependencyManagement dependencyManagement = project.getModel().getDependencyManagement();
        if (dependencyManagement == null) {
            return List.of();
        }
        return dependencyManagement.getDependencies();
    }

    private List<MicronautCoordinate> micronautParent(Parent parent) {
        if (parent == null || parent.getGroupId() == null || parent.getArtifactId() == null) {
            return List.of();
        }
        if (!isMicronautGroup(parent.getGroupId())) {
            return List.of();
        }
        String version = parent.getVersion();
        return List.of(new MicronautCoordinate(
            parent.getGroupId(),
            parent.getArtifactId(),
            version,
            "parent",
            parseMajorVersion(version)
        ));
    }

    private List<MicronautCoordinate> directMicronautDependencies(List<Dependency> dependencies, String source) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .filter(this::isDirectCompatibilitySignal)
            .map(dependency -> new MicronautCoordinate(
                dependency.getGroupId(),
                dependency.getArtifactId(),
                dependency.getVersion(),
                source,
                parseMajorVersion(dependency.getVersion())
            ))
            .toList();
    }

    private boolean isDirectCompatibilitySignal(Dependency dependency) {
        return dependency.getGroupId() != null
            && dependency.getArtifactId() != null
            && dependency.getVersion() != null
            && ((dependency.getGroupId().equals("io.micronaut.platform")
                && (dependency.getArtifactId().equals("micronaut-parent") || dependency.getArtifactId().equals("micronaut-platform")))
                || (dependency.getGroupId().equals("io.micronaut.starter") && dependency.getArtifactId().startsWith("micronaut-starter-"))
                || (dependency.getGroupId().equals("io.micronaut") && dependency.getArtifactId().startsWith("micronaut-")));
    }

    private boolean isMicronautGroup(String groupId) {
        return groupId != null && groupId.startsWith("io.micronaut");
    }

    private OptionalInt parseMajorVersion(String version) {
        if (version == null) {
            return OptionalInt.empty();
        }
        Matcher matcher = LEADING_MAJOR_PATTERN.matcher(version);
        if (!matcher.find()) {
            return OptionalInt.empty();
        }
        try {
            return OptionalInt.of(Integer.parseInt(matcher.group(1)));
        } catch (NumberFormatException e) {
            return OptionalInt.empty();
        }
    }

    private String buildFailureMessage(MavenProject project, int baselineMajor, List<MicronautCoordinate> coordinates, List<MicronautCoordinate> conflicts) {
        String baseline = coordinates.stream()
            .filter(coordinate -> coordinate.majorVersion().isPresent() && coordinate.majorVersion().getAsInt() == baselineMajor)
            .filter(coordinate -> "parent".equals(coordinate.source()) || "dependencymanagement".equals(coordinate.source()))
            .findFirst()
            .map(coordinate -> coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + ']')
            .orElse("major " + baselineMajor);

        String details = conflicts.stream()
            .sorted(Comparator
                .comparingInt((MicronautCoordinate coordinate) -> coordinate.majorVersion().orElseThrow())
                .thenComparing(MicronautCoordinate::groupId)
                .thenComparing(MicronautCoordinate::artifactId)
                .thenComparing(MicronautCoordinate::source))
            .map(coordinate -> "- " + coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + "] (major " + coordinate.majorVersion().orElseThrow() + ')')
            .collect(Collectors.joining(System.lineSeparator()));

        return "Mixed Micronaut major versions detected in project " + project.getArtifactId() + ". "
            + "Baseline major is " + baselineMajor + " from " + baseline + "."
            + System.lineSeparator()
            + System.lineSeparator()
            + details;
    }

    record MicronautCoordinate(
        String groupId,
        String artifactId,
        String version,
        String source,
        OptionalInt majorVersion
    ) {
        MicronautCoordinate {
            version = version == null ? "<unknown>" : version;
            source = source.toLowerCase(Locale.ROOT);
        }
    }
}
