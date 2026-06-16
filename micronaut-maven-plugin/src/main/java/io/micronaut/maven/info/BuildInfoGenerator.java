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
package io.micronaut.maven.info;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;

import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

/**
 * Generates build metadata properties.
 *
 * @author Micronaut Authors
 * @since 5.0.1
 */
final class BuildInfoGenerator {

    private static final String OUTPUT_TIMESTAMP_PROPERTY = "project.build.outputTimestamp";
    private static final String MICRONAUT_VERSION_PROPERTY = "micronaut.version";
    private static final String MAVEN_COMPILER_RELEASE_PROPERTY = "maven.compiler.release";
    private static final String MAVEN_COMPILER_SOURCE_PROPERTY = "maven.compiler.source";
    private static final String MAVEN_COMPILER_TARGET_PROPERTY = "maven.compiler.target";

    private BuildInfoGenerator() {
    }

    static Map<String, String> generate(MavenProject project, Instant buildTime, Map<String, String> additionalProperties) {
        var properties = new TreeMap<String, String>();
        putIfNotBlank(properties, "build.group", project.getGroupId());
        putIfNotBlank(properties, "build.artifact", project.getArtifactId());
        putIfNotBlank(properties, "build.name", project.getName());
        putIfNotBlank(properties, "build.version", project.getVersion());
        properties.put("build.time", buildTime.toString());

        Properties projectProperties = project.getProperties();
        putIfNotBlank(properties, "build.java.source", projectProperties.getProperty(MAVEN_COMPILER_SOURCE_PROPERTY));
        putIfNotBlank(properties, "build.java.target", projectProperties.getProperty(MAVEN_COMPILER_TARGET_PROPERTY));
        putIfNotBlank(properties, "build.java.release", projectProperties.getProperty(MAVEN_COMPILER_RELEASE_PROPERTY));
        putIfNotBlank(properties, "build.micronaut.version", projectProperties.getProperty(MICRONAUT_VERSION_PROPERTY));
        if (additionalProperties != null) {
            additionalProperties.forEach((key, value) -> putIfNotBlank(properties, key, value));
        }
        return properties;
    }

    static Instant resolveBuildTime(String configuredTime, MavenProject project, MavenSession session) {
        Instant configured = parseInstant(configuredTime);
        if (configured != null) {
            return configured;
        }

        Instant outputTimestamp = parseInstant(project.getProperties().getProperty(OUTPUT_TIMESTAMP_PROPERTY));
        if (outputTimestamp != null) {
            return outputTimestamp;
        }

        if (session != null && session.getRequest() != null) {
            Date startTime = session.getRequest().getStartTime();
            if (startTime != null) {
                return startTime.toInstant();
            }
        }
        return Instant.now();
    }

    private static Instant parseInstant(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        try {
            return Instant.parse(value.trim());
        } catch (DateTimeParseException e) {
            try {
                return Instant.ofEpochSecond(Long.parseLong(value.trim()));
            } catch (NumberFormatException ignored) {
                throw new IllegalArgumentException("Unsupported timestamp format: " + value, e);
            }
        }
    }

    private static void putIfNotBlank(Map<String, String> properties, String key, String value) {
        if (key != null && !key.isBlank() && value != null && !value.isBlank()) {
            properties.put(key, value);
        }
    }
}
