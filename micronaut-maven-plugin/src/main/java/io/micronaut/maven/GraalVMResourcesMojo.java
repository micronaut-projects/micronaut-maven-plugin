/*
 * Copyright 2017-2022 original authors
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

import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.apache.maven.model.Resource;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.resources.ResourcesMojo;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * <p>Generate GraalVM <code>resources-config.json</code> with all the resources included in the application</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly.</p>
 *
 * @author Iván López
 * @since 2.0
 */
@Mojo(name = GraalVMResourcesMojo.GRAALVM_RESOURCES)
public class GraalVMResourcesMojo extends ResourcesMojo {

    public static final String GRAALVM_RESOURCES = "graalvm-resources";

    private static final String META_INF = "META-INF";
    private static final String RESOURCES = "resources";
    private static final String PATTERN = "pattern";
    private static final String RESOURCE_CONFIG_JSON = "resource-config.json";
    private static final List<String> EXCLUDED_META_INF_DIRECTORIES = Arrays.asList("native-image", "services");

    private final ObjectWriter writer = new ObjectMapper().writer(new DefaultPrettyPrinter());

    @Parameter(property = "micronaut.native-image.skip-resources", defaultValue = "false")
    private Boolean nativeImageSkipResources;

    @Override
    public void execute() throws MojoExecutionException {
        if (Boolean.TRUE.equals(nativeImageSkipResources)) {
            getLog().info("Skipping generation of resource-config.json");
            return;
        }

        Set<String> resourcesToAdd = new HashSet<>();

        // Application resources (src/main/resources)
        for (Resource resource : getResources()) {
            resourcesToAdd.addAll(findResourceFiles(Paths.get(resource.getDirectory()).toFile()));
        }

        // Generated resources (like openapi)
        Path metaInfPath = getOutputDirectory().toPath().resolve(META_INF);
        resourcesToAdd.addAll(findResourceFiles(metaInfPath.toFile(), Collections.singletonList(META_INF)));

        Path nativeImagePath = buildNativeImagePath();
        Path graalVMResourcesPath = metaInfPath.resolve(nativeImagePath).toAbsolutePath();

        Map<String, Object> json = new HashMap<>();
        List<Map<String, String>> resourceList = resourcesToAdd.stream()
                .map(this::mapToGraalResource)
                .toList();

        json.put(RESOURCES, resourceList);

        try {
            Files.createDirectories(graalVMResourcesPath);
            File resourceConfigFile = graalVMResourcesPath.resolve(RESOURCE_CONFIG_JSON).toFile();
            getLog().info("Generating " + resourceConfigFile.getAbsolutePath());
            writer.writeValue(resourceConfigFile, json);

        } catch (IOException e) {
            throw new MojoExecutionException("There was an error generating GraalVM resource-config.json file", e);
        }
    }

    private Set<String> findResourceFiles(File folder) {
        return this.findResourceFiles(folder, null);
    }

    private Set<String> findResourceFiles(File folder, List<String> filePath) {
        Set<String> resourceFiles = new HashSet<>();

        if (filePath == null) {
            filePath = new ArrayList<>();
        }

        if (folder.exists()) {
            File[] files = folder.listFiles();

            if (files != null) {
                boolean isMetaInfDirectory = folder.getName().equals(META_INF);

                for (File element : files) {
                    boolean isExcludedDirectory = EXCLUDED_META_INF_DIRECTORIES.contains(element.getName());
                    // Exclude some directories in 'META-INF' like 'native-image' and 'services' but process other
                    // 'META-INF' files and directories, for example, to include swagger-ui.
                    if (!isMetaInfDirectory || !isExcludedDirectory) {
                        if (element.isDirectory()) {
                            List<String> paths = new ArrayList<>(filePath);
                            paths.add(element.getName());

                            resourceFiles.addAll(findResourceFiles(element, paths));
                        } else {
                            String joinedDirectories = String.join("/", filePath);
                            String elementName = joinedDirectories.isEmpty() ? element.getName() : joinedDirectories + "/" + element.getName();

                            resourceFiles.add(elementName);
                        }
                    }
                }
            }
        }

        return resourceFiles;
    }

    private Path buildNativeImagePath() {
        String group = project.getGroupId();
        String module = project.getArtifactId();

        return Paths.get("native-image", group, module);
    }

    private Map<String, String> mapToGraalResource(String resourceName) {
        Map<String, String> result = new HashMap<>();

        if (resourceName.contains("*")) {
            result.put(PATTERN, resourceName);
        } else {
            result.put(PATTERN, "\\Q" + resourceName + "\\E");
        }

        return result;
    }

    @Override
    public void setLog(Log log) {
        super.setLog(new JansiLog(log));
    }
}
