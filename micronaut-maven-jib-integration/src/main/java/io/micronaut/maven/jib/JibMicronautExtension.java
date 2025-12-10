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

import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FileEntry;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import com.google.cloud.tools.jib.api.buildplan.Platform;
import com.google.cloud.tools.jib.api.buildplan.Port;
import com.google.cloud.tools.jib.buildplan.UnixPathParser;
import com.google.cloud.tools.jib.maven.extension.JibMavenPluginExtension;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.services.ApplicationConfigurationService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Jib extension to support building Docker images.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public class JibMicronautExtension implements JibMavenPluginExtension<Void> {

    public static final String DEFAULT_JAVA21_BASE_IMAGE = "eclipse-temurin:21-jre";
    public static final String DEFAULT_JAVA25_BASE_IMAGE = "eclipse-temurin:25-jre";
    private static final String LATEST_TAG = "latest";
    private static final String JDK_TARGET_VERSION = "maven.compiler.target";
    private static final String JDK_RELEASE_VERSION = "maven.compiler.release";
    private static final String JDK_SOURCE_VERSION = "maven.compiler.source";
    private static final Logger LOG = LoggerFactory.getLogger(JibMicronautExtension.class);
    private static final String LINUX = "linux";

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

        var jibConfigurationService = new JibConfigurationService(mavenData.getMavenProject());

        String baseImage = buildPlan.getBaseImage();
        if (StringUtils.isEmpty(buildPlan.getBaseImage())) {
            baseImage = determineBaseImage(getJdkVersion(mavenData.getMavenSession()), runtime.getBuildStrategy());
            builder.setBaseImage(baseImage);
        }
        logger.log(ExtensionLogger.LogLevel.LIFECYCLE, "Using base image: " + baseImage);

        if (buildPlan.getExposedPorts() == null || buildPlan.getExposedPorts().isEmpty()) {
            var applicationConfigurationService = new ApplicationConfigurationService(mavenData.getMavenProject());
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
        }

        var detectedPlatform = detectPlatform();
        if (buildPlan.getPlatforms() == null || buildPlan.getPlatforms().isEmpty() || !buildPlan.getPlatforms().contains(detectedPlatform)) {
            LOG.info("Adding Detected platform: {}/{}", LINUX, detectedPlatform.getArchitecture());
            builder.addPlatform(detectedPlatform.getArchitecture(), LINUX);
        }

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
        var entrypoint = new ArrayList<String>(9);
        entrypoint.add("java");
        entrypoint.add("-XX:-UsePerfData");
        entrypoint.add("-XX:+UseSerialGC");
        entrypoint.add("-Xshare:auto");
        entrypoint.add("-Djava.awt.headless=true");
        entrypoint.add("-Djava.library.path=/function/runtime/lib");
        entrypoint.add("-cp");
        entrypoint.add("/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*");
        entrypoint.add("com.fnproject.fn.runtime.EntryPoint");
        return entrypoint;
    }

    public static String determineProjectFnVersion(String javaVersion) {
        int majorVersion = Integer.parseInt(javaVersion.split("\\.")[0]);
        if (majorVersion <= 25 && majorVersion > 21) {
            return "25-jre";
        } else if (majorVersion == 21) {
            return "21-jre";
        } else {
            return LATEST_TAG;
        }
    }

    public static String determineBaseImage(String jdkVersion, DockerBuildStrategy buildStrategy) {
        int javaVersion = Integer.parseInt(jdkVersion);
        return switch (buildStrategy) {
            case LAMBDA -> "public.ecr.aws/lambda/java:" + javaVersion;
            default -> javaVersion == 21 ? DEFAULT_JAVA21_BASE_IMAGE : DEFAULT_JAVA25_BASE_IMAGE;
        };
    }

    public static String getJdkVersion(MavenSession session) {
        var releaseVersion = getPropertyValue(session, JDK_RELEASE_VERSION);
        var targetVersion = getPropertyValue(session, JDK_TARGET_VERSION);
        var sourceVersion = getPropertyValue(session, JDK_SOURCE_VERSION);

        Optional<String> jdkVersionOpt = releaseVersion
            .or(() -> targetVersion)
            .or(() -> sourceVersion);

        String jdkVersion = jdkVersionOpt.orElse("21"); // Default to project baseline JDK 21
        String propertySource = releaseVersion.isPresent() ? JDK_RELEASE_VERSION :
                               targetVersion.isPresent() ? JDK_TARGET_VERSION :
                               sourceVersion.isPresent() ? JDK_SOURCE_VERSION : "default (21)";

        LOG.info("Using JDK version {} from {}", jdkVersion, propertySource);
        return jdkVersion;
    }

    private static Optional<String> getPropertyValue(MavenSession session, String propertName) {
        MojoExecution mojoExecution = new MojoExecution(new MojoDescriptor());
        var evaluator = new PluginParameterExpressionEvaluator(session, mojoExecution);
        try {
            return Optional.ofNullable((String) evaluator.evaluate("${" + propertName + "}", String.class));
        } catch (ExpressionEvaluationException e) {
            return Optional.empty();
        }
    }

    static LayerObject remapLayer(LayerObject layerObject) {
        var originalLayer = (FileEntriesLayer) layerObject;
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
        return new Platform(arch, LINUX);
    }

}
