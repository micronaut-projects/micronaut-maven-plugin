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
package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.buildplan.*;
import com.google.cloud.tools.jib.buildplan.UnixPathParser;
import com.google.cloud.tools.jib.maven.extension.JibMavenPluginExtension;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import io.micronaut.maven.AbstractDockerMojo;
import io.micronaut.maven.MicronautRuntime;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.JibConfigurationService;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

import java.util.*;

/**
 * Jib extension to support building Docker images.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public class JibMicronautExtension implements JibMavenPluginExtension<Void> {

    public static final String DEFAULT_BASE_IMAGE = "openjdk:17-alpine";

    @Override
    public Optional<Class<Void>> getExtraConfigType() {
        return Optional.empty();
    }

    @Override
    public ContainerBuildPlan extendContainerBuildPlan(ContainerBuildPlan buildPlan, Map<String, String> properties,
                                                       Optional<Void> extraConfig, MavenData mavenData,
                                                       ExtensionLogger logger) {

        ContainerBuildPlan.Builder builder = buildPlan.toBuilder();
        MicronautRuntime runtime = MicronautRuntime.valueOf(mavenData.getMavenProject().getProperties().getProperty(MicronautRuntime.PROPERTY, "none").toUpperCase());

        JibConfigurationService jibConfigurationService = new JibConfigurationService(mavenData.getMavenProject());
        String from = jibConfigurationService.getFromImage().orElse(DEFAULT_BASE_IMAGE);

        builder.setBaseImage(from);

        ApplicationConfigurationService applicationConfigurationService = new ApplicationConfigurationService(mavenData.getMavenProject());
        try {
            int port = Integer.parseInt(applicationConfigurationService.getServerPort());
            if (port > 0) {
                logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "Exposing port: " + port);
                builder.addExposedPort(Port.tcp(port));
            }
        } catch (NumberFormatException e) {
            // ignore, can't automatically expose port
            logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "Dynamically resolved port present. Ensure the port is correctly exposed in the <container> configuration. See https://github.com/GoogleContainerTools/jib/tree/master/jib-maven-plugin#example for an example.");
        }

        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                List<? extends LayerObject> originalLayers = buildPlan.getLayers();
                builder.setLayers(Collections.emptyList());

                for (LayerObject layer : originalLayers) {
                    builder.addLayer(remapLayer(layer));
                }

                List<String> cmd = jibConfigurationService.getArgs();
                if (cmd.isEmpty()) {
                    cmd = Collections.singletonList("io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest");
                }

                builder.setBaseImage("fnproject/fn-java-fdk:" + determineProjectFnVersion())
                        .setWorkingDirectory(AbsoluteUnixPath.get(jibConfigurationService.getWorkingDirectory().orElse("/function")))
                        .setEntrypoint(buildProjectFnEntrypoint())
                        .setCmd(cmd);
                break;

            case LAMBDA:
                List<String> entrypoint = buildPlan.getEntrypoint();
                Objects.requireNonNull(entrypoint).set(entrypoint.size() - 1, "io.micronaut.function.aws.runtime.MicronautLambdaRuntime");
                builder.setEntrypoint(entrypoint);
                break;

            default:
                //no op
        }
        return builder.build();
    }

    public static List<String> buildProjectFnEntrypoint() {
        List<String> entrypoint = new ArrayList<>(9);
        String projectFnVersion = determineProjectFnVersion();
        if (AbstractDockerMojo.LATEST_TAG.equals(projectFnVersion)) {
            entrypoint.add("java");
            entrypoint.add("-XX:+UnlockExperimentalVMOptions");
            entrypoint.add("-XX:+UseCGroupMemoryLimitForHeap");
            entrypoint.add("-XX:-UsePerfData");
            entrypoint.add("-XX:MaxRAMFraction=2");
            entrypoint.add("-XX:+UseSerialGC");
            entrypoint.add("-Xshare:on");
            entrypoint.add("-Djava.library.path=/function/runtime/lib");
            entrypoint.add("-cp");
            entrypoint.add("/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*");
            entrypoint.add("com.fnproject.fn.runtime.EntryPoint");
        } else {
            entrypoint.add("/usr/java/latest/bin/java");
            entrypoint.add("-XX:-UsePerfData");
            entrypoint.add("-XX:+UseSerialGC");
            entrypoint.add("-Xshare:on");
            entrypoint.add("-Djava.awt.headless=true");
            entrypoint.add("-Djava.library.path=/function/runtime/lib");
            entrypoint.add("-cp");
            entrypoint.add("/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*");
            entrypoint.add("com.fnproject.fn.runtime.EntryPoint");
        }
        return entrypoint;
    }

    public static String determineProjectFnVersion() {
        ArtifactVersion javaVersion = new DefaultArtifactVersion(System.getProperty("java.version"));
        if (javaVersion.getMajorVersion() >= 17) {
            return "jre17-latest";
        } else {
            return AbstractDockerMojo.LATEST_TAG;
        }
    }

    private LayerObject remapLayer(LayerObject layerObject) {
        FileEntriesLayer originalLayer = (FileEntriesLayer) layerObject;
        FileEntriesLayer.Builder builder = FileEntriesLayer.builder().setName(originalLayer.getName());
        for (FileEntry originalEntry : originalLayer.getEntries()) {
            builder.addEntry(remapEntry(originalEntry, layerObject.getName()));
        }

        return builder.build();
    }

    private FileEntry remapEntry(FileEntry originalEntry, String layerName) {
        List<String> pathComponents = UnixPathParser.parse(originalEntry.getExtractionPath().toString());
        AbsoluteUnixPath newPath;
        if (layerName.contains("dependencies")) {
            newPath = AbsoluteUnixPath.get("/function/app/libs/" + pathComponents.get(pathComponents.size() - 1));
        } else {
            //classes or resources
            newPath = AbsoluteUnixPath.get("/function" + originalEntry.getExtractionPath());
        }

        return new FileEntry(originalEntry.getSourceFile(), newPath, originalEntry.getPermissions(),
                originalEntry.getModificationTime(), originalEntry.getOwnership());
    }

}
