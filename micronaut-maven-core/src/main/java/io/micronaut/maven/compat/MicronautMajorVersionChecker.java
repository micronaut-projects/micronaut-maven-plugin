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
package io.micronaut.maven.compat;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Parent;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.OptionalInt;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class MicronautMajorVersionChecker {

    private static final Pattern LEADING_MAJOR_PATTERN = Pattern.compile("^(\\d+)");

    public List<MicronautMajorVersionCoordinate> collectEnforcerCoordinates(
        List<Dependency> dependencies,
        List<Dependency> dependencyManagementDependencies,
        List<Dependency> annotationProcessorPaths
    ) {
        return Stream.of(
                collectMicronautDependencies(dependencies, "dependency"),
                collectMicronautDependencies(dependencyManagementDependencies, "dependencyManagement"),
                collectMicronautDependencies(annotationProcessorPaths, "annotationProcessorPath")
            )
            .flatMap(List::stream)
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .distinct()
            .toList();
    }

    public List<MicronautMajorVersionCoordinate> collectLifecycleCoordinates(
        Parent parent,
        List<Dependency> dependencies,
        List<Dependency> dependencyManagementDependencies
    ) {
        return Stream.of(
                collectLifecycleParent(parent),
                collectLifecycleBaselineSignals(dependencyManagementDependencies),
                collectDirectCompatibilitySignals(dependencies, "dependency")
            )
            .flatMap(List::stream)
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .distinct()
            .toList();
    }

    public MicronautMajorVersionAnalysis analyzeForLifecycle(List<MicronautMajorVersionCoordinate> coordinates) {
        OptionalInt baselineMajor = coordinates.stream()
            .filter(this::isMicronautBaselineCoordinate)
            .map(MicronautMajorVersionCoordinate::majorVersion)
            .filter(OptionalInt::isPresent)
            .mapToInt(OptionalInt::getAsInt)
            .findFirst();

        if (baselineMajor.isEmpty()) {
            return new MicronautMajorVersionAnalysis(coordinates, OptionalInt.empty(), List.of());
        }

        List<MicronautMajorVersionCoordinate> conflicts = coordinates.stream()
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .filter(coordinate -> coordinate.majorVersion().getAsInt() != baselineMajor.getAsInt())
            .sorted(defaultOrdering())
            .toList();

        return new MicronautMajorVersionAnalysis(coordinates, baselineMajor, conflicts);
    }

    public boolean hasMixedMajors(List<MicronautMajorVersionCoordinate> coordinates) {
        return coordinates.stream()
            .map(MicronautMajorVersionCoordinate::majorVersion)
            .filter(OptionalInt::isPresent)
            .mapToInt(OptionalInt::getAsInt)
            .distinct()
            .count() > 1;
    }

    public OptionalInt parseMajorVersion(String version) {
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

    public String buildEnforcerFailureMessage(List<MicronautMajorVersionCoordinate> coordinates) {
        String details = coordinates.stream()
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .collect(Collectors.groupingBy(
                coordinate -> coordinate.majorVersion().orElseThrow(),
                java.util.LinkedHashMap::new,
                Collectors.toList()
            ))
            .entrySet().stream()
            .map(entry -> "- major " + entry.getKey() + ':' + System.lineSeparator() + entry.getValue().stream()
                .sorted(defaultOrdering())
                .map(coordinate -> "  - " + coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + ']')
                .collect(Collectors.joining(System.lineSeparator())))
            .collect(Collectors.joining(System.lineSeparator()));

        return "Mixed Micronaut major versions detected in the build configuration. "
            + "Make sure dependencies, dependencyManagement entries, and annotation processor paths all use the same Micronaut major version."
            + System.lineSeparator()
            + System.lineSeparator()
            + details;
    }

    public String buildLifecycleFailureMessage(String projectArtifactId, MicronautMajorVersionAnalysis analysis) {
        int baselineMajor = analysis.baselineMajor().orElseThrow();
        String baseline = analysis.coordinates().stream()
            .filter(coordinate -> coordinate.majorVersion().isPresent() && coordinate.majorVersion().getAsInt() == baselineMajor)
            .filter(this::isMicronautBaselineCoordinate)
            .findFirst()
            .map(coordinate -> coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + ']')
            .orElse("major " + baselineMajor);

        String details = analysis.conflicts().stream()
            .map(coordinate -> "- " + coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + "] (major " + coordinate.majorVersion().orElseThrow() + ')')
            .collect(Collectors.joining(System.lineSeparator()));

        return "Mixed Micronaut major versions detected in project " + projectArtifactId + ". "
            + "Baseline major is " + baselineMajor + " from " + baseline + "."
            + System.lineSeparator()
            + System.lineSeparator()
            + details;
    }

    public List<MicronautMajorVersionCoordinate> collectMicronautDependencies(List<Dependency> dependencies, String source) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .filter(this::isMicronautDependency)
            .map(dependency -> new MicronautMajorVersionCoordinate(
                dependency.getGroupId(),
                dependency.getArtifactId(),
                dependency.getVersion(),
                source,
                parseMajorVersion(dependency.getVersion())
            ))
            .toList();
    }

    public List<MicronautMajorVersionCoordinate> collectDirectCompatibilitySignals(List<Dependency> dependencies, String source) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .filter(this::isDirectCompatibilitySignal)
            .map(dependency -> new MicronautMajorVersionCoordinate(
                dependency.getGroupId(),
                dependency.getArtifactId(),
                dependency.getVersion(),
                source,
                parseMajorVersion(dependency.getVersion())
            ))
            .toList();
    }

    public List<MicronautMajorVersionCoordinate> collectLifecycleBaselineSignals(List<Dependency> dependencies) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .filter(this::isLifecycleBaselineSignal)
            .map(dependency -> new MicronautMajorVersionCoordinate(
                dependency.getGroupId(),
                dependency.getArtifactId(),
                dependency.getVersion(),
                "dependencyManagement",
                parseMajorVersion(dependency.getVersion())
            ))
            .toList();
    }

    public List<MicronautMajorVersionCoordinate> collectLifecycleParent(Parent parent) {
        if (parent == null || parent.getGroupId() == null || parent.getArtifactId() == null) {
            return List.of();
        }
        if (!("io.micronaut.platform".equals(parent.getGroupId())
            && ("micronaut-parent".equals(parent.getArtifactId()) || "micronaut-platform".equals(parent.getArtifactId())))) {
            return List.of();
        }
        return List.of(new MicronautMajorVersionCoordinate(
            parent.getGroupId(),
            parent.getArtifactId(),
            parent.getVersion(),
            "parent",
            parseMajorVersion(parent.getVersion())
        ));
    }

    private boolean isLifecycleBaselineSignal(Dependency dependency) {
        return dependency.getGroupId() != null
            && dependency.getArtifactId() != null
            && dependency.getVersion() != null
            && (("io.micronaut.platform".equals(dependency.getGroupId())
                && ("micronaut-parent".equals(dependency.getArtifactId()) || "micronaut-platform".equals(dependency.getArtifactId())))
                || ("io.micronaut".equals(dependency.getGroupId()) && "micronaut-core-bom".equals(dependency.getArtifactId())));
    }

    private boolean isMicronautBaselineCoordinate(MicronautMajorVersionCoordinate coordinate) {
        return ("parent".equals(coordinate.source()) || "dependencymanagement".equals(coordinate.source()))
            && (("io.micronaut.platform".equals(coordinate.groupId())
                && ("micronaut-parent".equals(coordinate.artifactId()) || "micronaut-platform".equals(coordinate.artifactId())))
                || ("io.micronaut".equals(coordinate.groupId()) && "micronaut-core-bom".equals(coordinate.artifactId())));
    }

    private boolean isMicronautDependency(Dependency dependency) {
        return dependency.getGroupId() != null
            && dependency.getArtifactId() != null
            && isMicronautGroup(dependency.getGroupId());
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

    private Comparator<MicronautMajorVersionCoordinate> defaultOrdering() {
        return Comparator
            .comparingInt((MicronautMajorVersionCoordinate coordinate) -> coordinate.majorVersion().orElseThrow())
            .thenComparing(MicronautMajorVersionCoordinate::groupId)
            .thenComparing(MicronautMajorVersionCoordinate::artifactId)
            .thenComparing(MicronautMajorVersionCoordinate::source);
    }
}
