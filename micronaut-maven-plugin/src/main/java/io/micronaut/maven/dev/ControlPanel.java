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
package io.micronaut.maven.dev;

import org.apache.maven.artifact.versioning.ComparableVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.transfer.ArtifactNotFoundException;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * The Micronaut Control Panel {@code mn:dev} adds to the development runtime: the UI, the management panels and the
 * testing panels of continuous testing, and the optional panels whose library the application already depends on,
 * as Pyronaut infers them. What the application declares itself is never added again.
 *
 * <p>Its settings are written as {@value #PROPERTIES_RESOURCE} into a directory of the development runtime's
 * classpath: Control Panel's {@code ControlPanelContextConfigurer} loads every such resource as the property source of
 * lowest precedence, so that whatever the application configures, in a configuration file, a system property or an
 * environment variable, wins.</p>
 *
 * @since 5.1.0
 */
public final class ControlPanel {

    /**
     * The group of every Control Panel module.
     */
    public static final String GROUP = "io.micronaut.controlpanel";

    /**
     * The modules added to every development runtime: the UI, the management panels, and the testing panels.
     */
    public static final List<String> BASE_MODULES = List.of(
        "micronaut-control-panel-ui",
        "micronaut-control-panel-management",
        "micronaut-control-panel-testing"
    );

    /**
     * The name of the directory beside the development manifest holding {@link #PROPERTIES_RESOURCE}, which the
     * runtime classpath holds.
     */
    public static final String RESOURCES_DIRECTORY_NAME = "control-panel";

    /**
     * The resource Control Panel reads its defaults from, at the lowest precedence.
     */
    public static final String PROPERTIES_RESOURCE = "micronaut-control-panel/micronaut-control-panel.properties";

    /**
     * The settings of the development runtime, defaults the application's own configuration overrides: those of an
     * application without Micronaut Security, which is how Control Panel is open in the development environment
     * anyway.
     */
    public static final Map<String, String> DEFAULT_PROPERTIES;

    /**
     * The settings of the development runtime of an application that depends on Micronaut Security: no access
     * setting, so that the application's security decides who opens the panels.
     */
    public static final Map<String, String> SECURED_PROPERTIES;

    /**
     * The module whose presence on the runtime classpath leaves the access to Control Panel to the application.
     */
    public static final String SECURITY_MODULE = "io.micronaut.security:micronaut-security";

    /**
     * The group of the Micronaut platform BOM and of the {@code micronaut-parent} POM.
     */
    public static final String PLATFORM_GROUP = "io.micronaut.platform";

    /**
     * The oldest Micronaut platform {@code mn:dev} adds Control Panel for: Control Panel requires Micronaut Platform
     * 5.2.4 or later.
     */
    public static final String MINIMUM_PLATFORM_VERSION = "5.2.4";

    private static final List<String> PLATFORM_ARTIFACTS = List.of("micronaut-parent", "micronaut-platform");

    static {
        Map<String, String> secured = new LinkedHashMap<>();
        secured.put("micronaut.control-panel.enabled", "true");
        secured.put("micronaut.control-panel.path", "/control-panel");
        SECURED_PROPERTIES = Collections.unmodifiableMap(secured);
        Map<String, String> properties = new LinkedHashMap<>(secured);
        properties.put("micronaut.control-panel.security.access", "ANONYMOUS");
        DEFAULT_PROPERTIES = Collections.unmodifiableMap(properties);
    }

    private ControlPanel() {
    }

    /**
     * The optional panels, each added when the application depends on one of the modules the panel inspects. Kept
     * in line with Pyronaut's {@code ControlPanelFeature} and the Gradle plugin.
     */
    public enum Feature {
        DATASOURCE("micronaut-control-panel-datasource", List.of("io.micronaut.sql:micronaut-jdbc")),
        HIBERNATE("micronaut-control-panel-hibernate", List.of("org.hibernate.orm:hibernate-core")),
        KAFKA("micronaut-control-panel-kafka", List.of("io.micronaut.kafka:micronaut-kafka")),
        OBJECT_STORAGE("micronaut-control-panel-object-storage", List.of("io.micronaut.objectstorage:micronaut-object-storage-core")),
        CACHE("micronaut-control-panel-cache", List.of(
            "io.micronaut.cache:micronaut-cache-caffeine",
            "io.micronaut.cache:micronaut-cache-ehcache",
            "io.micronaut.cache:micronaut-cache-hazelcast",
            "io.micronaut.cache:micronaut-cache-infinispan"
        ));

        private final String artifactId;
        private final List<String> triggers;

        Feature(String artifactId, List<String> triggers) {
            this.artifactId = artifactId;
            this.triggers = triggers;
        }

        /**
         * @return the panel's artifact id
         */
        public String artifactId() {
            return artifactId;
        }

        /**
         * @return the {@code group:artifact} keys whose presence adds the panel
         */
        public List<String> triggers() {
            return triggers;
        }
    }

    /**
     * The version of the Micronaut platform a project builds with: that of {@code micronaut-parent} in the project's
     * effective dependency management, which every {@code micronaut-platform} BOM manages at its own version, whether
     * the project inherits it from {@code micronaut-parent} or imports it, from a profile or with an interpolated
     * version; else that of {@code micronaut-parent} or of the {@code micronaut-platform} BOM among its parents.
     *
     * @param project the project
     * @return the version, if the project builds with the Micronaut platform
     */
    public static Optional<String> platformVersion(MavenProject project) {
        DependencyManagement management = project.getDependencyManagement();
        if (management != null) {
            for (Dependency dependency : management.getDependencies()) {
                if (PLATFORM_GROUP.equals(dependency.getGroupId()) && "micronaut-parent".equals(dependency.getArtifactId())
                    && dependency.getVersion() != null) {
                    return Optional.of(dependency.getVersion());
                }
            }
        }
        for (MavenProject current = project; current != null; current = current.getParent()) {
            if (PLATFORM_GROUP.equals(current.getGroupId()) && PLATFORM_ARTIFACTS.contains(current.getArtifactId())) {
                return Optional.ofNullable(current.getVersion());
            }
        }
        return Optional.empty();
    }

    /**
     * Whether a Micronaut platform version is {@value #MINIMUM_PLATFORM_VERSION} or later, compared as Maven compares
     * versions, so that {@code 5.2.10} is later and {@code 5.2.4-SNAPSHOT} earlier.
     *
     * @param version the version of the platform
     * @return whether Control Panel is added for it
     */
    public static boolean isSupportedPlatform(String version) {
        return version != null && !version.isBlank()
            && new ComparableVersion(version.trim()).compareTo(new ComparableVersion(MINIMUM_PLATFORM_VERSION)) >= 0;
    }

    /**
     * The modules to add for a runtime classpath: the base modules, then the panels its modules enable, in the order
     * of {@link Feature}.
     *
     * @param moduleKeys the {@code group:artifact} keys of the runtime classpath
     * @return the artifact ids, in group {@link #GROUP}
     */
    public static List<String> modules(Collection<String> moduleKeys) {
        List<String> modules = new ArrayList<>(BASE_MODULES);
        for (Feature feature : Feature.values()) {
            if (feature.triggers.stream().anyMatch(moduleKeys::contains)) {
                modules.add(feature.artifactId);
            }
        }
        return modules;
    }

    /**
     * The settings for a runtime classpath: with Micronaut Security, none that opens the panels to anonymous users.
     *
     * @param moduleKeys the {@code group:artifact} keys of the runtime classpath
     * @return {@link #SECURED_PROPERTIES} or {@link #DEFAULT_PROPERTIES}
     */
    public static Map<String, String> properties(Collection<String> moduleKeys) {
        return moduleKeys.contains(SECURITY_MODULE) ? SECURED_PROPERTIES : DEFAULT_PROPERTIES;
    }

    /**
     * Writes {@link #PROPERTIES_RESOURCE} with the {@link #properties(Collection) settings} of a runtime classpath
     * under a directory.
     *
     * @param directory the directory, which the runtime classpath holds
     * @param moduleKeys the {@code group:artifact} keys of the runtime classpath
     * @throws IOException if it cannot be written
     */
    public static void writeResources(Path directory, Collection<String> moduleKeys) throws IOException {
        Path file = directory.resolve(PROPERTIES_RESOURCE);
        Files.createDirectories(file.getParent());
        Properties properties = new Properties();
        properties.putAll(properties(moduleKeys));
        try (OutputStream out = Files.newOutputStream(file)) {
            properties.store(out, "Written by the Micronaut Maven Plugin for mn:dev: defaults the application's configuration overrides");
        }
    }

    /**
     * Deletes the directory {@link #writeResources(Path, Collection)} wrote, when the Control Panel is off.
     *
     * @param directory the directory
     * @throws IOException if it cannot be deleted
     */
    public static void deleteResources(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (Stream<Path> paths = Files.walk(directory)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.delete(path);
            }
        }
    }

    /**
     * Whether a resolution failed only because Control Panel artifacts are not published, such as the testing
     * panels before their first release: the module is left out. Any other failure, an artifact of another group or a
     * repository error, is not tolerated.
     *
     * @param failure the failure
     * @return true when every artifact that failed is a missing one of {@link #GROUP}
     */
    public static boolean isMissingControlPanelArtifact(Throwable failure) {
        for (Throwable cause = failure; cause != null; cause = cause.getCause()) {
            if (cause instanceof ArtifactResolutionException resolution) {
                List<ArtifactResult> failed = resolution.getResults().stream().filter(result -> !result.isResolved()).toList();
                return !failed.isEmpty() && failed.stream().allMatch(ControlPanel::isMissing);
            }
        }
        return false;
    }

    private static boolean isMissing(ArtifactResult result) {
        org.eclipse.aether.artifact.Artifact artifact = result.getRequest().getArtifact();
        return artifact != null && GROUP.equals(artifact.getGroupId())
            && !result.getExceptions().isEmpty()
            && result.getExceptions().stream().allMatch(ArtifactNotFoundException.class::isInstance);
    }
}
