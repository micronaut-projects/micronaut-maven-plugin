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
import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.services.ApplicationConfigurationService;
import org.apache.maven.project.MavenProject;

import java.util.*;

/**
 * Jib extension to support building Docker images.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public class JibMicronautExtension implements JibMavenPluginExtension<Void> {

    public static final String DEFAULT_JAVA17_BASE_IMAGE = "eclipse-temurin:17-jre";
    public static final String DEFAULT_JAVA21_BASE_IMAGE = "eclipse-temurin:21-jre";
    private static final String LATEST_TAG = "latest";
    private static final String JDK_VERSION = "maven.compiler.target";

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

        String baseImage = buildPlan.getBaseImage();
        if (StringUtils.isEmpty(buildPlan.getBaseImage())) {
            baseImage = determineBaseImage(getJdkVersion(mavenData.getMavenProject()), runtime.getBuildStrategy());
            builder.setBaseImage(baseImage);
        }
        logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "Using base image: " + baseImage);

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

        builder.setPlatforms(Set.of(detectPlatform()));

        switch (runtime.getBuildStrategy()) {
            case ORACLE_FUNCTION -> {
                List<? extends LayerObject> originalLayers = buildPlan.getLayers();
                builder.setLayers(originalLayers.stream().map(JibMicronautExtension::remapLayer).toList());
                List<String> cmd = jibConfigurationService.getArgs();
                if (cmd.isEmpty()) {
                    cmd = Collections.singletonList("io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest");
                }
                builder.setWorkingDirectory(AbsoluteUnixPath.get(jibConfigurationService.getWorkingDirectory().orElse("/function")))
                        .setEntrypoint(buildProjectFnEntrypoint())
                        .setCmd(cmd);
            }
            case LAMBDA -> {
                //TODO Leverage AWS Base images:
                // https://docs.aws.amazon.com/lambda/latest/dg/java-image.html
                // https://docs.aws.amazon.com/lambda/latest/dg/images-create.html
                // https://docs.aws.amazon.com/lambda/latest/dg/images-test.html
                List<String> entrypoint = buildPlan.getEntrypoint();
                Objects.requireNonNull(entrypoint).set(entrypoint.size() - 1, "io.micronaut.function.aws.runtime.MicronautLambdaRuntime");
                builder.setEntrypoint(entrypoint);
            }
            default -> {
                //no op
            }
        }
        return builder.build();
    }

    public static List<String> buildProjectFnEntrypoint() {
        List<String> entrypoint = new ArrayList<>(9);
        entrypoint.add("/usr/java/latest/bin/java");
        entrypoint.add("-XX:-UsePerfData");
        entrypoint.add("-XX:+UseSerialGC");
        entrypoint.add("-Xshare:on");
        entrypoint.add("-Djava.awt.headless=true");
        entrypoint.add("-Djava.library.path=/function/runtime/lib");
        entrypoint.add("-cp");
        entrypoint.add("/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*");
        entrypoint.add("com.fnproject.fn.runtime.EntryPoint");
        return entrypoint;
    }

    public static String determineProjectFnVersion(String javaVersion) {
        int majorVersion = Integer.parseInt(javaVersion.split("\\.")[0]);
        if (majorVersion >= 17) {
            return "jre17-latest";
        } else {
            return LATEST_TAG;
        }
    }

    public static String determineBaseImage(String jdkVersion, DockerBuildStrategy buildStrategy) {
        int javaVersion = Integer.parseInt(jdkVersion);
        return switch (buildStrategy) {
            case ORACLE_FUNCTION -> "fnproject/fn-java-fdk:" + determineProjectFnVersion(System.getProperty("java.version"));
            case LAMBDA -> "public.ecr.aws/lambda/java:" + javaVersion;
            default -> javaVersion == 17 ? DEFAULT_JAVA17_BASE_IMAGE : DEFAULT_JAVA21_BASE_IMAGE;
        };
    }

    public static String getJdkVersion(MavenProject project) {
        return System.getProperty(JDK_VERSION, project.getProperties().getProperty(JDK_VERSION));
    }

    static LayerObject remapLayer(LayerObject layerObject) {
        FileEntriesLayer originalLayer = (FileEntriesLayer) layerObject;
        FileEntriesLayer.Builder builder = FileEntriesLayer.builder().setName(originalLayer.getName());
        for (FileEntry originalEntry : originalLayer.getEntries()) {
            builder.addEntry(remapEntry(originalEntry, layerObject.getName()));
        }

        return builder.build();
    }

    static FileEntry remapEntry(FileEntry originalEntry, String layerName) {
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

    private Platform detectPlatform() {
        String arch = System.getProperty("os.arch").equals("aarch64") ? "arm64" : "amd64";
        return new Platform(arch, "linux");
    }

}
