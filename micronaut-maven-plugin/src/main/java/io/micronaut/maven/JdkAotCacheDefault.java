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
package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfiguration;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Stream;

/**
 * The checks that switch off the JDK AOT cache that the {@code docker} packaging trains by default, when
 * {@code micronaut.docker.jdkAotCache} is not set. Each one keeps today's image where training a cache would change
 * what the build asked for.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.2.0
 */
final class JdkAotCacheDefault {

    static final String OPTION = "micronaut.docker.jdkAotCache";
    static final String PACKAGED = "packaged";

    private static final String LOG_PREFIX = "JDK AOT cache: ";
    private static final String CONTAINERIZING_MODE = "containerizingMode";
    private static final String CONTAINERIZING_MODE_PROPERTY = "jib.containerizingMode";
    private static final String EXTRA_DIRECTORIES_PROPERTY = "jib.extraDirectories.paths";
    private static final String APP_ROOT_PROPERTY = "jib.container.appRoot";
    private static final String DEFAULT_APP_ROOT = "/app";
    private static final String DEFAULT_EXTRA_DIRECTORY = "src/main/jib";
    private static final String LINUX = "linux";
    private static final String AMD64 = "amd64";
    private static final String ARM64 = "arm64";

    private JdkAotCacheDefault() {
    }

    /**
     * @param log the Maven log
     */
    static void logTrained(Log log) {
        log.info(LOG_PREFIX + "the image trains one by default, which makes the application start faster. Set -D"
            + OPTION + "=false to build the image without it, or -D" + OPTION + "=true to make a training failure fail "
            + "the build");
    }

    /**
     * @param log the Maven log
     * @param reason why the cache is not trained
     */
    static void logSkipped(Log log, String reason) {
        log.info(LOG_PREFIX + "not trained by default, because " + reason + ". Set -D" + OPTION + "=true to train one");
    }

    /**
     * @param log the Maven log
     * @param reason why the training failed
     * @param warn whether this is a failure, or a choice of the build, such as a base image with an older Java
     */
    static void logFallback(Log log, String reason, boolean warn) {
        String message = LOG_PREFIX + "the image is built without the cache that it trains by default, because " + reason
            + ". Set -D" + OPTION + "=true to make this an error, or -D" + OPTION + "=false to skip the training";
        if (warn) {
            log.warn(message);
        } else {
            log.info(message);
        }
    }

    /**
     * @param jibEntrypoint the Jib {@code container.entrypoint}
     * @param project the Maven project
     * @param userProperties the user properties of the session
     * @param systemProperties the system properties of the session
     * @return why the Jib build cannot train a cache by default, or {@code null}
     */
    static @Nullable String jibGuard(List<String> jibEntrypoint, MavenProject project, Properties userProperties,
                                     Properties systemProperties) {
        if (!jibEntrypoint.isEmpty()) {
            return "the Jib container.entrypoint is set, and the cache needs Jib's default java entrypoint";
        }
        Xpp3Dom configuration = jibConfiguration(project);
        String mode = jibProperty(CONTAINERIZING_MODE_PROPERTY, project, userProperties, systemProperties);
        if (mode == null) {
            mode = childValue(configuration, CONTAINERIZING_MODE);
        }
        if (mode != null && !PACKAGED.equals(mode.trim())) {
            return "the Jib containerizing mode is " + mode.trim() + ", and the cache needs " + PACKAGED
                + ", because it cannot use classes from a directory";
        }
        String appRoot = jibProperty(APP_ROOT_PROPERTY, project, userProperties, systemProperties);
        if (appRoot == null) {
            Xpp3Dom container = configuration == null ? null : configuration.getChild("container");
            appRoot = childValue(container, "appRoot");
        }
        String classPathFile = extraDirectoryOnClassPath(project, configuration,
            jibProperty(EXTRA_DIRECTORIES_PROPERTY, project, userProperties, systemProperties),
            appRoot == null || appRoot.isBlank() ? DEFAULT_APP_ROOT : appRoot.trim());
        if (classPathFile != null) {
            return "the Jib extra directories put " + classPathFile + " in a class path directory of Jib's exploded "
                + "mode, which the " + PACKAGED + " mode of the cache leaves out of the class path";
        }
        if (!hasJarPlugin(project)) {
            return "the project does not declare the maven-jar-plugin, which builds the application JAR of the cache";
        }
        return null;
    }

    /**
     * @param daemonPlatform the {@code os/architecture} of the Docker daemon
     * @param configured the Jib {@code from.platforms}
     * @param jibBuildGoal the Jib goal
     * @param hostArchitecture the {@code os.arch} of the build JVM, which the Jib extension adds as a platform
     * @return why the image would lose a platform with the cache, or {@code null}
     */
    static @Nullable String platformGuard(String daemonPlatform, Set<JibConfiguration.PlatformConfiguration> configured,
                                          String jibBuildGoal, String hostArchitecture) {
        Set<String> platforms = new LinkedHashSet<>();
        for (JibConfiguration.PlatformConfiguration platform : configured) {
            platforms.add(platform.os().orElse(LINUX) + "/" + goArchitecture(platform.architecture().orElse(AMD64)));
        }
        if (AbstractDockerMojo.JIB_BUILD_GOAL_DOCKER_BUILD.equals(jibBuildGoal)) {
            // With several platforms, Jib only builds the one of the Docker engine
            if (platforms.isEmpty() || platforms.contains(daemonPlatform)) {
                return null;
            }
            return "the Jib from.platforms " + platforms + " do not include " + daemonPlatform
                + ", the platform of the Docker daemon that trains the cache";
        }
        if (platforms.isEmpty()) {
            platforms.add(LINUX + "/" + AMD64);
        }
        platforms.add(LINUX + "/" + goArchitecture(hostArchitecture));
        if (platforms.equals(Set.of(daemonPlatform))) {
            return null;
        }
        return "the image is built for " + platforms + ", and a cache only works on " + daemonPlatform
            + ", the platform of the Docker daemon that trains it";
    }

    /**
     * @param architecture an architecture of the Jib {@code from.platforms} or of {@code os.arch}
     * @return the Go architecture name, as the platform of the Docker daemon gives it
     */
    static String goArchitecture(String architecture) {
        return switch (architecture.toLowerCase(Locale.ROOT)) {
            case "x86_64", AMD64 -> AMD64;
            case "aarch64", ARM64 -> ARM64;
            default -> architecture;
        };
    }

    /**
     * @param project the Maven project
     * @return whether the build plugins or the plugin management have the maven-jar-plugin, which the cache build runs
     */
    static boolean hasJarPlugin(MavenProject project) {
        String key = "org.apache.maven.plugins:maven-jar-plugin";
        if (project.getPlugin(key) != null) {
            return true;
        }
        return project.getBuild() != null && project.getBuild().getPluginManagement() != null
            && project.getBuild().getPluginManagement().getPlugins().stream().anyMatch(plugin -> key.equals(plugin.getKey()));
    }

    /**
     * Looks for a file of the Jib extra directories that lands in {@code <appRoot>/classes} or
     * {@code <appRoot>/resources}, where Jib's exploded mode has its class path directories.
     *
     * @return the path of the first such file in the image, or {@code null}
     */
    static @Nullable String extraDirectoryOnClassPath(MavenProject project, @Nullable Xpp3Dom configuration,
                                                      @Nullable String pathsProperty, String appRoot) {
        String root = appRoot.endsWith("/") ? appRoot.substring(0, appRoot.length() - 1) : appRoot;
        List<String> classPath = List.of(root + "/classes/", root + "/resources/");
        for (ExtraDirectory directory : extraDirectories(configuration, pathsProperty)) {
            Path from = project.getBasedir().toPath().resolve(directory.from());
            if (!Files.isDirectory(from)) {
                continue;
            }
            String into = directory.into().endsWith("/") ? directory.into() : directory.into() + "/";
            try (Stream<Path> files = Files.walk(from)) {
                String found = files.filter(Files::isRegularFile)
                    .map(file -> into + from.relativize(file).toString().replace('\\', '/'))
                    .filter(target -> classPath.stream().anyMatch(target::startsWith))
                    .findFirst()
                    .orElse(null);
                if (found != null) {
                    return found;
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        return null;
    }

    /**
     * @return the {@code from} and {@code into} of each extra directory, with Jib's default when none is configured
     */
    private static List<ExtraDirectory> extraDirectories(@Nullable Xpp3Dom configuration, @Nullable String pathsProperty) {
        var result = new ArrayList<ExtraDirectory>();
        if (pathsProperty != null) {
            for (String path : pathsProperty.split(",")) {
                if (!path.isBlank()) {
                    result.add(new ExtraDirectory(path.trim(), "/"));
                }
            }
            return result;
        }
        Xpp3Dom extraDirectories = configuration == null ? null : configuration.getChild("extraDirectories");
        Xpp3Dom paths = extraDirectories == null ? null : extraDirectories.getChild("paths");
        if (paths == null) {
            result.add(new ExtraDirectory(DEFAULT_EXTRA_DIRECTORY, "/"));
            return result;
        }
        for (Xpp3Dom path : paths.getChildren("path")) {
            String from = childValue(path, "from");
            if (from != null) {
                String into = childValue(path, "into");
                result.add(new ExtraDirectory(from.trim(), into == null || into.isBlank() ? "/" : into.trim()));
            } else if (path.getValue() != null && !path.getValue().isBlank()) {
                result.add(new ExtraDirectory(path.getValue().trim(), "/"));
            }
        }
        return result;
    }

    private static @Nullable Xpp3Dom jibConfiguration(MavenProject project) {
        Plugin plugin = project.getPlugin("com.google.cloud.tools:jib-maven-plugin");
        return plugin != null && plugin.getConfiguration() instanceof Xpp3Dom configuration ? configuration : null;
    }

    private static @Nullable String childValue(@Nullable Xpp3Dom parent, String name) {
        Xpp3Dom child = parent == null ? null : parent.getChild(name);
        return child == null || child.getValue() == null || child.getValue().isBlank() ? null : child.getValue();
    }

    /**
     * Resolves a Jib property as the Jib Maven plugin does: user properties, project properties, then system
     * properties.
     */
    private static @Nullable String jibProperty(String name, MavenProject project, Properties userProperties,
                                                Properties systemProperties) {
        for (Properties properties : List.of(userProperties, project.getProperties(), systemProperties)) {
            if (properties != null && properties.containsKey(name)) {
                return properties.getProperty(name);
            }
        }
        return null;
    }

    /**
     * A Jib extra directory.
     *
     * @param from the directory of the project
     * @param into where its files go in the image
     */
    private record ExtraDirectory(String from, String into) {
    }
}
