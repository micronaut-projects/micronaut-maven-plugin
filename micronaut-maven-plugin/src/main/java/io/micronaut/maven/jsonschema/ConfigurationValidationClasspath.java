/*
 * Copyright 2017-2026 original authors
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
package io.micronaut.maven.jsonschema;

import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Default classpath computation for configuration validation scenarios.
 */
final class ConfigurationValidationClasspath {
    private ConfigurationValidationClasspath() {
    }

    /**
     * Default classpath for {@code mn:run} validation.
     *
     * @param project The Maven project
     * @return Classpath elements
     */
    static List<String> defaultDevClasspath(MavenProject project) {
        // Requirement: src/main/resources + src/main/java
        return defaultMainSourceClasspath(project);
    }

    /**
     * Default classpath for {@code package} validation.
     *
     * @param project The Maven project
     * @return Classpath elements
     */
    static List<String> defaultPackageClasspath(MavenProject project) {
        // Requirement: allow validating during package using main source classpath.
        return defaultMainSourceClasspath(project);
    }

    /**
     * Default classpath for {@code test} validation.
     *
     * @param project The Maven project
     * @return Classpath elements
     */
    static List<String> defaultTestClasspath(MavenProject project) {
        Set<String> result = new LinkedHashSet<>();
        result.addAll(defaultMainSourceClasspath(project));
        addResources(result, project.getBuild().getTestResources());
        addIfExists(result, project.getBasedir().toPath().resolve("src/test/resources").toString());
        addIfExists(result, project.getBuild().getTestOutputDirectory());
        return new ArrayList<>(result);
    }

    private static List<String> defaultMainSourceClasspath(MavenProject project) {
        Set<String> result = new LinkedHashSet<>();
        addResources(result, project.getBuild().getResources());
        Path basedir = project.getBasedir().toPath();
        addIfExists(result, basedir.resolve("src/main/resources").toString());
        addIfExists(result, project.getBuild().getOutputDirectory());
        return new ArrayList<>(result);
    }

    private static void addResources(Set<String> classpath, List<Resource> resources) {
        if (resources == null) {
            return;
        }
        for (Resource r : resources) {
            if (r != null && r.getDirectory() != null) {
                addIfExists(classpath, r.getDirectory());
            }
        }
    }

    private static void addIfExists(Set<String> classpath, String path) {
        if (path == null || path.isBlank()) {
            return;
        }
        File f = new File(path);
        if (f.exists()) {
            classpath.add(f.getAbsolutePath());
        }
    }
}
