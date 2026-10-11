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

import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * The Micronaut Control Panel {@code mn:dev} adds to the development runtime: the UI and the management panels, the
 * testing panels of continuous testing, and the optional panels whose library the application already depends on, as
 * Pyronaut infers them. What the application declares itself is never added again, and the settings the goal passes
 * are defaults: one the application configures is left to it.
 *
 * @since 5.1.0
 */
public final class ControlPanel {

    /**
     * The group of every Control Panel module.
     */
    public static final String GROUP = "io.micronaut.controlpanel";

    /**
     * The modules added to every development runtime.
     */
    public static final List<String> BASE_MODULES = List.of("micronaut-control-panel-ui", "micronaut-control-panel-management");

    /**
     * The testing panels of continuous testing, added when the dependency management versions them.
     */
    public static final String TESTING_MODULE = "micronaut-control-panel-testing";

    /**
     * The key that enables the Control Panel.
     */
    public static final String ENABLED = "micronaut.control-panel.enabled";

    /**
     * The key of the path the Control Panel is served at.
     */
    public static final String PATH = "micronaut.control-panel.path";

    /**
     * The key of who may access the Control Panel.
     */
    public static final String SECURITY_ACCESS = "micronaut.control-panel.security.access";

    /**
     * The name of the JVM argument file beside the development manifest holding the settings passed, for a build that
     * launches the JVM itself: {@code java @control-panel.argfile ...}.
     */
    public static final String ARGUMENT_FILE_NAME = "control-panel.argfile";

    /**
     * The environment of the development runtime.
     */
    public static final String DEVELOPMENT_ENVIRONMENT = "dev";

    /**
     * The settings passed to the development runtime unless the application configures them.
     */
    public static final Map<String, String> DEFAULT_PROPERTIES;

    static {
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put(ENABLED, "true");
        properties.put(PATH, "/control-panel");
        properties.put(SECURITY_ACCESS, "ANONYMOUS");
        DEFAULT_PROPERTIES = Collections.unmodifiableMap(properties);
    }

    /**
     * The configuration files Micronaut reads from the classpath: {@code application} and those of an environment.
     */
    private static final Pattern CONFIGURATION_FILE = Pattern.compile("application(?:-([^.]+))?\\.(properties|ya?ml|json|toml|groovy|conf)");

    private ControlPanel() {
    }

    /**
     * The optional panels, each added when the application depends on one of the modules the panel inspects. Kept
     * in line with Pyronaut's {@code ControlPanelFeature}.
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
     * The modules to add for a runtime classpath: the base modules, then the panels its modules enable, in the order
     * of {@link Feature}. The testing module is not among them, since it is resolved on its own.
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
     * The settings to pass: the defaults the application does not configure.
     *
     * @param configured whether the application configures a key
     * @return the settings, in a stable order
     */
    public static Map<String, String> properties(Predicate<String> configured) {
        Map<String, String> properties = new LinkedHashMap<>();
        DEFAULT_PROPERTIES.forEach((key, value) -> {
            if (!configured.test(key)) {
                properties.put(key, value);
            }
        });
        return properties;
    }

    /**
     * Whether JVM arguments set a system property.
     *
     * @param key the property
     * @param arguments the arguments
     * @return true for a {@code -Dkey} or {@code -Dkey=value} among them
     */
    public static boolean isSystemProperty(String key, Collection<String> arguments) {
        String property = "-D" + key;
        return arguments.stream().anyMatch(argument -> argument.equals(property) || argument.startsWith(property + "="));
    }

    /**
     * The environment variable Micronaut reads a key from: {@code MICRONAUT_CONTROL_PANEL_PATH} for
     * {@code micronaut.control-panel.path}.
     *
     * @param key the property
     * @return the variable's name
     */
    public static String environmentVariable(String key) {
        return key.toUpperCase(Locale.ROOT).replace('.', '_').replace('-', '_');
    }

    /**
     * The Control Panel keys the configuration files of resource directories set: every {@code application} file,
     * and that of each active environment, read as properties, YAML or JSON; the file of an environment that is not
     * active is not loaded by the runtime, so it configures nothing. A file of another format, TOML or Groovy,
     * sets a key when it names its last segment within the {@code control-panel} section, a guess that only ever
     * leaves a default out.
     *
     * @param resourceDirectories the resource directories
     * @param environments the active environments
     * @return the keys of {@link #DEFAULT_PROPERTIES} they set
     */
    public static Set<String> configuredKeys(Collection<Path> resourceDirectories, Collection<String> environments) {
        Set<String> keys = new HashSet<>();
        for (Path directory : resourceDirectories) {
            if (!Files.isDirectory(directory)) {
                continue;
            }
            try (Stream<Path> files = Files.list(directory)) {
                for (Path file : files.filter(file -> isConfigurationFile(file, environments)).sorted().toList()) {
                    keys.addAll(configuredKeys(file));
                }
            } catch (IOException e) {
                // an unreadable directory configures nothing
            }
        }
        return keys;
    }

    private static boolean isConfigurationFile(Path file, Collection<String> environments) {
        Matcher matcher = CONFIGURATION_FILE.matcher(file.getFileName().toString());
        if (!Files.isRegularFile(file) || !matcher.matches()) {
            return false;
        }
        String environment = matcher.group(1);
        return environment == null || environments.contains(environment);
    }

    /**
     * The environments of the development runtime: {@code dev}, which the launcher activates, and those a
     * {@code micronaut.environments} setting names.
     *
     * @param environmentsSetting the value of {@code micronaut.environments} or {@code MICRONAUT_ENVIRONMENTS}, if any
     * @return the environments
     */
    public static Set<String> environments(String environmentsSetting) {
        Set<String> environments = new LinkedHashSet<>();
        environments.add(DEVELOPMENT_ENVIRONMENT);
        if (environmentsSetting != null) {
            for (String environment : environmentsSetting.split(",")) {
                if (!environment.isBlank()) {
                    environments.add(environment.trim());
                }
            }
        }
        return environments;
    }

    /**
     * The value of a system property among JVM arguments.
     *
     * @param key the property
     * @param arguments the arguments
     * @return the value of the last {@code -Dkey=value}, or null
     */
    public static String systemProperty(String key, List<String> arguments) {
        String property = "-D" + key + "=";
        String value = null;
        for (String argument : arguments) {
            if (argument.startsWith(property)) {
                value = argument.substring(property.length());
            }
        }
        return value;
    }

    private static String extension(String name) {
        return name.substring(name.lastIndexOf('.') + 1).toLowerCase(Locale.ROOT);
    }

    static Set<String> configuredKeys(Path file) {
        Set<String> found = new HashSet<>();
        String extension = extension(file.getFileName().toString());
        try {
            Set<String> keys = new HashSet<>();
            switch (extension) {
                case "properties" -> {
                    Properties properties = new Properties();
                    try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
                        properties.load(reader);
                    }
                    properties.stringPropertyNames().forEach(key -> keys.add(normalize(key)));
                }
                case "yml", "yaml", "json" -> {
                    Yaml yaml = new Yaml(new SafeConstructor(new LoaderOptions()));
                    try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
                        for (Object document : yaml.loadAll(reader)) {
                            flatten("", document, keys);
                        }
                    }
                }
                default -> {
                    String text = Files.readString(file, StandardCharsets.UTF_8);
                    int section = Math.max(text.indexOf("control-panel"), text.indexOf("controlPanel"));
                    if (section >= 0) {
                        String rest = text.substring(section);
                        for (String key : DEFAULT_PROPERTIES.keySet()) {
                            if (rest.contains(key.substring(key.lastIndexOf('.') + 1))) {
                                found.add(key);
                            }
                        }
                    }
                }
            }
            for (String key : DEFAULT_PROPERTIES.keySet()) {
                if (keys.contains(key)) {
                    found.add(key);
                }
            }
        } catch (IOException | RuntimeException e) {
            // a file that cannot be read or parsed configures nothing the goal can see; the application reports it
        }
        return found;
    }

    private static void flatten(String prefix, Object value, Set<String> keys) {
        if (value instanceof Map<?, ?> map) {
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                String key = prefix.isEmpty() ? normalize(String.valueOf(entry.getKey())) : prefix + "." + normalize(String.valueOf(entry.getKey()));
                flatten(key, entry.getValue(), keys);
            }
        } else if (!prefix.isEmpty()) {
            keys.add(prefix);
        }
    }

    /**
     * A key as Micronaut binds it: kebab case, lower case.
     */
    private static String normalize(String key) {
        StringBuilder normalized = new StringBuilder(key.length() + 4);
        for (int i = 0; i < key.length(); i++) {
            char c = key.charAt(i);
            if (Character.isUpperCase(c)) {
                if (i > 0 && key.charAt(i - 1) != '.' && key.charAt(i - 1) != '-') {
                    normalized.append('-');
                }
                normalized.append(Character.toLowerCase(c));
            } else {
                normalized.append(c);
            }
        }
        return normalized.toString();
    }
}
