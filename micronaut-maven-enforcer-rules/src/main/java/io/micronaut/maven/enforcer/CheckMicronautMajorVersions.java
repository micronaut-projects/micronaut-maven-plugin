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

import org.apache.maven.enforcer.rule.api.AbstractEnforcerRule;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Objects;
import java.util.OptionalInt;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
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
    private static final Pattern LEADING_MAJOR_PATTERN = Pattern.compile("^(\\d+)");

    private final MavenProject project;

    @Inject
    public CheckMicronautMajorVersions(MavenProject project) {
        this.project = project;
    }

    @Override
    public void execute() throws EnforcerRuleException {
        List<MicronautCoordinate> coordinates = Stream.of(
                micronautDependencies(project.getDependencies(), "dependency"),
                micronautDependencies(project.getDependencyManagement() != null ? project.getDependencyManagement().getDependencies() : List.of(), "dependencyManagement"),
                micronautDependencies(compilerAnnotationProcessorPaths(), "annotationProcessorPath")
            )
            .flatMap(List::stream)
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .distinct()
            .toList();

        Set<Integer> detectedMajors = coordinates.stream()
            .map(MicronautCoordinate::majorVersion)
            .filter(OptionalInt::isPresent)
            .mapToInt(OptionalInt::getAsInt)
            .boxed()
            .collect(Collectors.toCollection(LinkedHashSet::new));

        if (detectedMajors.size() > 1) {
            throw new EnforcerRuleException(buildFailureMessage(coordinates));
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

    private List<MicronautCoordinate> micronautDependencies(List<Dependency> dependencies, String source) {
        return dependencies.stream()
            .filter(Objects::nonNull)
            .filter(this::isMicronautDependency)
            .map(dependency -> {
                String resolvedVersion = resolveVersion(dependency.getVersion());
                return new MicronautCoordinate(
                    dependency.getGroupId(),
                    dependency.getArtifactId(),
                    resolvedVersion,
                    source,
                    parseMajorVersion(resolvedVersion)
                );
            })
            .toList();
    }

    private boolean isMicronautDependency(Dependency dependency) {
        return dependency.getGroupId() != null
            && dependency.getArtifactId() != null
            && dependency.getGroupId().startsWith("io.micronaut");
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

    private String buildFailureMessage(List<MicronautCoordinate> coordinates) {
        Map<Integer, List<MicronautCoordinate>> byMajor = coordinates.stream()
            .filter(coordinate -> coordinate.majorVersion().isPresent())
            .sorted(Comparator
                .comparingInt((MicronautCoordinate coordinate) -> coordinate.majorVersion().orElseThrow())
                .thenComparing(MicronautCoordinate::groupId)
                .thenComparing(MicronautCoordinate::artifactId)
                .thenComparing(MicronautCoordinate::source))
            .collect(Collectors.groupingBy(
                coordinate -> coordinate.majorVersion().orElseThrow(),
                LinkedHashMap::new,
                Collectors.toList()
            ));

        String details = byMajor.entrySet().stream()
            .map(entry -> "- major " + entry.getKey() + ':' + System.lineSeparator() + entry.getValue().stream()
                .map(coordinate -> "  - " + coordinate.groupId() + ':' + coordinate.artifactId() + ':' + coordinate.version() + " [" + coordinate.source() + ']')
                .collect(Collectors.joining(System.lineSeparator())))
            .collect(Collectors.joining(System.lineSeparator()));

        return "Mixed Micronaut major versions detected in the build configuration. "
            + "Make sure dependencies, dependencyManagement entries, and annotation processor paths all use the same Micronaut major version."
            + System.lineSeparator()
            + System.lineSeparator()
            + details;
    }

    @Override
    public String toString() {
        return "CheckMicronautMajorVersions";
    }

    private record MicronautCoordinate(
        String groupId,
        String artifactId,
        String version,
        String source,
        OptionalInt majorVersion
    ) {
        private MicronautCoordinate {
            version = version == null ? "<unknown>" : version;
            source = source.toLowerCase(Locale.ROOT);
        }
    }
}
