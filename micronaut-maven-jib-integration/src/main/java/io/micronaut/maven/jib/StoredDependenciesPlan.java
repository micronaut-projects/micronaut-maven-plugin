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

import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import com.google.cloud.tools.jib.buildplan.UnixPathParser;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.core.StoredJars;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Replaces the dependency JARs of a Jib build plan with copies whose entries are all STORED.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
final class StoredDependenciesPlan {

    /**
     * The directory, set by the {@code docker} goal, where the STORED copies are written.
     */
    static final String DIRECTORY_PROPERTY = "micronaut.docker.storedDependencies.internal.directory";

    /**
     * The names of the layers that Jib puts the dependencies in.
     */
    static final Set<String> DEPENDENCY_LAYERS = Set.of("dependencies", "snapshot dependencies", "project dependencies");

    private static final String JAR_EXTENSION = ".jar";

    private StoredDependenciesPlan() {
    }

    /**
     * Replaces the dependency layers of the plan when the {@code docker} goal asked for STORED dependencies.
     *
     * @param buildPlan the build plan
     * @param builder the builder of the new plan
     * @param mavenData the Maven data
     * @param logger the extension logger
     * @throws JibPluginExtensionException if a JAR cannot be copied
     */
    static void apply(ContainerBuildPlan buildPlan, ContainerBuildPlan.Builder builder, MavenData mavenData,
                      ExtensionLogger logger) throws JibPluginExtensionException {
        String directory = mavenData.getMavenProject().getProperties().getProperty(DIRECTORY_PROPERTY);
        if (StringUtils.isEmpty(directory)) {
            return;
        }
        try {
            builder.setLayers(storeLayers(buildPlan.getLayers(), Path.of(directory)));
        } catch (IOException e) {
            throw new JibPluginExtensionException(JibMicronautExtension.class,
                "Could not write the STORED copies of the dependency JARs: " + e.getMessage(), e);
        }
        logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "Writing the dependency JARs with STORED entries");
    }

    /**
     * @param layers the layers
     * @param directory the directory for the copies
     * @return the layers, with the JARs of the dependency layers replaced by their STORED copies
     * @throws IOException if a JAR cannot be copied
     */
    static List<LayerObject> storeLayers(List<? extends LayerObject> layers, Path directory) throws IOException {
        var result = new ArrayList<LayerObject>(layers.size());
        for (LayerObject layer : layers) {
            if (layer instanceof FileEntriesLayer fileLayer && DEPENDENCY_LAYERS.contains(fileLayer.getName())) {
                result.add(storeLayer(fileLayer, directory.resolve(fileLayer.getName().replace(' ', '-'))));
            } else {
                result.add(layer);
            }
        }
        return result;
    }

    private static FileEntriesLayer storeLayer(FileEntriesLayer layer, Path directory) throws IOException {
        FileEntriesLayer.Builder builder = FileEntriesLayer.builder().setName(layer.getName());
        for (FileEntry entry : layer.getEntries()) {
            // Jib gives each dependency a distinct file name in the image, so it also names the copy
            List<String> components = UnixPathParser.parse(entry.getExtractionPath().toString());
            String name = components.get(components.size() - 1);
            if (name.endsWith(JAR_EXTENSION)) {
                Path copy = directory.resolve(name);
                StoredJars.store(entry.getSourceFile(), copy);
                builder.addEntry(new FileEntry(copy, entry.getExtractionPath(), entry.getPermissions(),
                    entry.getModificationTime(), entry.getOwnership()));
            } else {
                builder.addEntry(entry);
            }
        }
        return builder.build();
    }
}
