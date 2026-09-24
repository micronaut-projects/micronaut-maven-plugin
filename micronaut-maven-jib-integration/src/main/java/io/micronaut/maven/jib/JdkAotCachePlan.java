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
package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.CacheDirectoryCreationException;
import com.google.cloud.tools.jib.api.Containerizer;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.Platform;
import com.google.cloud.tools.jib.cache.Cache;
import com.google.cloud.tools.jib.cache.CacheCorruptedException;
import com.google.cloud.tools.jib.image.json.ImageMetadataTemplate;
import com.google.cloud.tools.jib.image.json.ManifestListTemplate;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import io.micronaut.core.annotation.Internal;
import io.micronaut.core.util.StringUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Changes the Jib build plan of the {@code docker} packaging when it trains a JDK AOT cache
 * ({@code micronaut.docker.jdkAotCache}). The {@code docker} goal drives these changes through project properties: the
 * extension builds for the Docker daemon's platform only, requires an explicit garbage collector and, in the final build,
 * adds the trained cache and the {@code -XX:AOTCache} flag, with the base image pinned to the digest the training build
 * pulled.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public final class JdkAotCachePlan {

    /**
     * The {@code os/architecture} of the Docker daemon that trains the cache. The {@code docker} goal sets it for both
     * the training build and the final build.
     */
    public static final String PLATFORM_PROPERTY = "micronaut.docker.jdkAotCache.internal.platform";

    /**
     * The trained cache on the build host. The {@code docker} goal sets it for the final build only.
     */
    public static final String CACHE_FILE_PROPERTY = "micronaut.docker.jdkAotCache.internal.file";

    /**
     * Whether the final build pins a registry base image to the digest that the training build pulled.
     */
    public static final String PIN_BASE_IMAGE_PROPERTY = "micronaut.docker.jdkAotCache.internal.pinBaseImage";

    /**
     * The name of the layer that holds the cache.
     */
    public static final String LAYER_NAME = "jdk aot cache";

    /**
     * The file name of the cache in the image, next to Jib's class path file.
     */
    public static final String CACHE_FILE_NAME = "app.aot";

    static final String BASE_IMAGE_CACHE_PROPERTY = "jib.baseImageCache";
    private static final Pattern GARBAGE_COLLECTOR_FLAG = Pattern.compile("-XX:\\+Use(Serial|Parallel|G1|Z|Shenandoah|Epsilon)GC");
    private static final List<String> JVM_OPTIONS_VARIABLES = List.of("JAVA_TOOL_OPTIONS", "JDK_JAVA_OPTIONS");
    private static final String JIB_CLASSPATH_FILE = "jib-classpath-file";
    private static final String DEFAULT_APP_ROOT = "/app";

    private JdkAotCachePlan() {
    }

    /**
     * @param mavenData the Maven project and session
     * @return the platform of the Docker daemon that trains the cache, if the {@code docker} goal is training one
     * @throws JibPluginExtensionException if the property is not an {@code os/architecture} pair
     */
    static Optional<Platform> platform(MavenData mavenData) throws JibPluginExtensionException {
        String value = mavenData.getMavenProject().getProperties().getProperty(PLATFORM_PROPERTY);
        if (StringUtils.isEmpty(value)) {
            return Optional.empty();
        }
        String[] parts = value.split("/", 2);
        if (parts.length != 2 || parts[0].isBlank() || parts[1].isBlank()) {
            throw new JibPluginExtensionException(JibMicronautExtension.class, "Invalid " + PLATFORM_PROPERTY + ": " + value);
        }
        return Optional.of(new Platform(parts[1], parts[0]));
    }

    /**
     * Applies the JDK AOT cache changes to a {@code DEFAULT} strategy build plan.
     *
     * @param buildPlan the plan as Jib created it
     * @param baseImage the base image of the plan
     * @param builder the builder of the plan the extension returns
     * @param platform the Docker daemon's platform
     * @param mavenData the Maven project and session
     * @param logger the extension logger
     * @throws JibPluginExtensionException if the plan cannot use a JDK AOT cache
     */
    static void apply(ContainerBuildPlan buildPlan, String baseImage, ContainerBuildPlan.Builder builder, Platform platform,
                      MavenData mavenData, ExtensionLogger logger) throws JibPluginExtensionException {
        builder.setPlatforms(Set.of(platform));
        logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "JDK AOT cache: building for the Docker daemon's platform "
            + platform.getOs() + "/" + platform.getArchitecture() + " only");

        List<String> entrypoint = buildPlan.getEntrypoint();
        if (entrypoint == null || entrypoint.isEmpty() || !"java".equals(entrypoint.get(0))) {
            throw new JibPluginExtensionException(JibMicronautExtension.class, "micronaut.docker.jdkAotCache needs "
                + "Jib's default java entrypoint, but the entrypoint is " + entrypoint);
        }
        if (!selectsGarbageCollector(entrypoint, buildPlan.getEnvironment())) {
            throw new JibPluginExtensionException(JibMicronautExtension.class, "micronaut.docker.jdkAotCache needs an "
                + "explicit garbage collector in the Jib container.jvmFlags, for example -XX:+UseSerialGC or "
                + "-XX:+UseG1GC. Parts of a JDK AOT cache depend on the collector that trained it, and without the "
                + "flag the JVM picks the collector from the CPUs and memory it finds, which usually differ between "
                + "the build and the deployment.");
        }

        var properties = mavenData.getMavenProject().getProperties();
        String cacheFile = properties.getProperty(CACHE_FILE_PROPERTY);
        if (StringUtils.isEmpty(cacheFile)) {
            return;
        }
        if (Boolean.parseBoolean(properties.getProperty(PIN_BASE_IMAGE_PROPERTY))) {
            Optional<String> pinned = pinnedBaseImage(baseImage, platform, baseImageCacheDirectory(mavenData));
            if (pinned.isPresent()) {
                builder.setBaseImage(pinned.get());
                logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "JDK AOT cache: using base image " + pinned.get()
                    + ", the one the cache was trained on");
            } else {
                logger.log(ExtensionLogger.LogLevel.WARN, "JDK AOT cache: could not find the digest of "
                    + baseImage + " that the training build pulled, so the base image is not pinned");
            }
        }
        AbsoluteUnixPath cachePath = appRoot(entrypoint).resolve(CACHE_FILE_NAME);
        builder.addLayer(FileEntriesLayer.builder()
            .setName(LAYER_NAME)
            .addEntry(Paths.get(cacheFile), cachePath, FilePermissions.DEFAULT_FILE_PERMISSIONS,
                FileEntriesLayer.DEFAULT_MODIFICATION_TIME)
            .build());
        var withCache = new ArrayList<>(entrypoint);
        withCache.add(1, "-XX:AOTCache=" + cachePath);
        builder.setEntrypoint(withCache);
        logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "JDK AOT cache: adding " + cachePath + " to the image");
    }

    /**
     * @param entrypoint the entrypoint
     * @param environment the image environment
     * @return whether the JVM options select a garbage collector
     */
    static boolean selectsGarbageCollector(List<String> entrypoint, Map<String, String> environment) {
        for (String argument : entrypoint) {
            if (GARBAGE_COLLECTOR_FLAG.matcher(argument).find()) {
                return true;
            }
        }
        if (environment != null) {
            for (String variable : JVM_OPTIONS_VARIABLES) {
                String value = environment.get(variable);
                if (value != null && GARBAGE_COLLECTOR_FLAG.matcher(value).find()) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param entrypoint Jib's java entrypoint
     * @return the application root, taken from Jib's class path file argument
     */
    static AbsoluteUnixPath appRoot(List<String> entrypoint) {
        for (String argument : entrypoint) {
            if (argument.startsWith("@/") && argument.endsWith("/" + JIB_CLASSPATH_FILE)) {
                String root = argument.substring(1, argument.length() - JIB_CLASSPATH_FILE.length() - 1);
                return AbsoluteUnixPath.get(root.isEmpty() ? "/" : root);
            }
        }
        return AbsoluteUnixPath.get(DEFAULT_APP_ROOT);
    }

    /**
     * Looks up, in Jib's base image cache, the platform manifest that the training build pulled for a registry base
     * image referenced by tag.
     *
     * @param baseImage the base image of the build plan
     * @param platform the platform
     * @param cacheDirectory Jib's base image cache
     * @return the base image pinned to the digest of the platform manifest, if the cache has it
     */
    static Optional<String> pinnedBaseImage(String baseImage, Platform platform, Path cacheDirectory) {
        if (StringUtils.isEmpty(baseImage) || !Files.isDirectory(cacheDirectory)) {
            return Optional.empty();
        }
        try {
            ImageReference reference = ImageReference.parse(baseImage);
            if (reference.isScratch() || reference.getDigest().isPresent()) {
                return Optional.empty();
            }
            Optional<ImageMetadataTemplate> metadata = Cache.withDirectory(cacheDirectory).retrieveMetadata(reference);
            if (metadata.isEmpty() || !(metadata.get().getManifestList() instanceof ManifestListTemplate manifestList)) {
                return Optional.empty();
            }
            return manifestList.getDigestsForPlatform(platform.getArchitecture(), platform.getOs())
                .stream()
                .findFirst()
                .map(digest -> ImageReference.of(reference.getRegistry(), reference.getRepository(),
                    reference.getTag().orElse(null), digest).toString());
        } catch (InvalidImageReferenceException | CacheDirectoryCreationException | CacheCorruptedException
                 | IOException | RuntimeException e) {
            return Optional.empty();
        }
    }

    /**
     * Resolves {@code jib.baseImageCache} with the precedence of the Jib Maven plugin: user properties, project
     * properties, then system properties.
     */
    private static Path baseImageCacheDirectory(MavenData mavenData) {
        String directory = Stream.of(mavenData.getMavenSession().getUserProperties(),
                mavenData.getMavenProject().getProperties(), mavenData.getMavenSession().getSystemProperties())
            .filter(properties -> properties != null && properties.containsKey(BASE_IMAGE_CACHE_PROPERTY))
            .map(properties -> properties.getProperty(BASE_IMAGE_CACHE_PROPERTY))
            .findFirst()
            .orElse(null);
        return StringUtils.isEmpty(directory) ? Containerizer.DEFAULT_BASE_CACHE_DIRECTORY : Paths.get(directory);
    }
}
