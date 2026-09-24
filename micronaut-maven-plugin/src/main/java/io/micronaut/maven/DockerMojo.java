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

import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.exception.NotFoundException;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jdkaotcache.JdkAotCacheDockerContext;
import io.micronaut.maven.jdkaotcache.JdkAotCacheTraining;
import io.micronaut.maven.jdkaotcache.TrainingRunSwitch;
import io.micronaut.maven.jib.JdkAotCachePlan;
import io.micronaut.maven.jib.JibConfiguration;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static io.micronaut.maven.DockerfileMojo.DOCKERFILE_ORACLE_CLOUD;

/**
 * <p>Allows using a provided Dockerfile.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 * using the <code>packaging</code> property, eg:</p>
 *
 * <pre>mvn package -Dpackaging=docker</pre>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = DockerMojo.DOCKER_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerMojo extends AbstractDockerMojo {

    public static final String DOCKER_PACKAGING = "docker";

    private static final String JIB_PLUGIN = "com.google.cloud.tools:jib-maven-plugin";
    private static final String DOCKER_URI_PREFIX = "docker://";
    private static final String TAR_URI_PREFIX = "tar://";
    private static final String PACKAGED = "packaged";

    private final ExecutorService executorService;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                      ApplicationConfigurationService applicationConfigurationService, DockerService dockerService,
                      MavenSession mavenSession, MojoExecution mojoExecution, ExecutorService executorService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession,
            mojoExecution);
        this.executorService = executorService;
    }

    @Override
    public void execute() throws MojoExecutionException {
        var providedDockerfile = new File(mavenProject.getBasedir(), DockerfileMojo.DOCKERFILE);
        if (jdkAotCache && MicronautRuntime.valueOf(micronautRuntime.toUpperCase()).getBuildStrategy() != DockerBuildStrategy.DEFAULT) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache only supports the default runtime, not "
                + micronautRuntime);
        }
        if (shouldBuildWithDockerfile(providedDockerfile)) {
            var dockerfile = determineDockerfile(providedDockerfile);
            buildDockerfile(dockerfile, providedDockerfile.exists());
        } else {
            validateJibBuildGoal();
            if (JIB_BUILD_GOAL_BUILD.equals(jibBuildGoal)) {
                requireConfiguredToImageForJibRegistryBuild();
            }
            if (jibConfigurationService.getFromImage().isEmpty()) {
                mavenProject.getProperties().setProperty(PropertyNames.FROM_IMAGE, getBaseImage());
            }
            if (jdkAotCache) {
                buildWithJdkAotCache();
            } else {
                executorService.executeGoal(mavenProject, JIB_PLUGIN, jibBuildGoal);
            }
        }
    }

    /**
     * Builds a training image with Jib in packaged mode, trains a JDK AOT cache with it and builds the image again with
     * the configured goal. Jib layers are reproducible, so the JARs of the final image match the trained ones in size
     * and modification time, which the cache checks, and the extension adds the cache as the last layer.
     */
    private void buildWithJdkAotCache() throws MojoExecutionException {
        List<String> trainingPaths = validateJdkAotCacheConfiguration();
        String platform = daemonPlatform();
        requireDaemonPlatform(platform);
        boolean useSwitch = TrainingRunSwitch.isAvailable(mavenProject.getArtifacts(), !trainingPaths.isEmpty());

        Path workDirectory = Path.of(mavenProject.getBuild().getDirectory(), JdkAotCacheDockerContext.CONTEXT_DIRECTORY);
        Path cacheFile = workDirectory.resolve(JdkAotCachePlan.CACHE_FILE_NAME);
        Properties properties = mavenProject.getProperties();
        Map<String, String> savedProperties = saveProperties(properties, PropertyNames.CONTAINERIZING_MODE,
            PropertyNames.TO_IMAGE, JdkAotCachePlan.PLATFORM_PROPERTY, JdkAotCachePlan.CACHE_FILE_PROPERTY,
            JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY);
        String trainingImageId = null;
        try {
            Files.createDirectories(workDirectory);
            Files.deleteIfExists(cacheFile);
            properties.setProperty(PropertyNames.CONTAINERIZING_MODE, PACKAGED);
            properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, platform);

            properties.setProperty(PropertyNames.TO_IMAGE, mavenProject.getArtifactId().toLowerCase(Locale.ROOT) + "-jdk-aot-training");
            String trainingImage = targetImage();
            getLog().info("JDK AOT cache: building the training image " + trainingImage + " for " + platform);
            executorService.executeGoal(mavenProject, JIB_PLUGIN, JIB_BUILD_GOAL_DOCKER_BUILD);
            trainingImageId = inspectImage(trainingImage)
                .orElseThrow(() -> new MojoExecutionException("JDK AOT cache: Jib did not build the training image "
                    + trainingImage + ". Is jib.skip set?"))
                .getId();
            restoreProperty(properties, PropertyNames.TO_IMAGE, savedProperties.get(PropertyNames.TO_IMAGE));

            new JdkAotCacheTraining(dockerService, getLog(), trainingPaths, jdkAotCacheTrainingTimeout, networkMode)
                .train(trainingImageId, useSwitch, cacheFile);

            properties.setProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY, cacheFile.toAbsolutePath().toString());
            properties.setProperty(JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY, String.valueOf(isRegistryBaseImage()));
            getLog().info("JDK AOT cache: building the image with the cache (jib:" + jibBuildGoal + ")");
            executorService.executeGoal(mavenProject, JIB_PLUGIN, jibBuildGoal);
            if (JIB_BUILD_GOAL_DOCKER_BUILD.equals(jibBuildGoal)) {
                verifyFinalImageLayers(trainingImageId, targetImage());
            }
        } catch (IOException e) {
            throw new MojoExecutionException("JDK AOT cache: " + e.getMessage(), e);
        } finally {
            restoreProperties(properties, savedProperties);
            if (trainingImageId != null) {
                removeTrainingImage(trainingImageId);
            }
        }
    }

    private List<String> validateJdkAotCacheConfiguration() throws MojoExecutionException {
        if (!jibConfigurationService.getEntrypoint().isEmpty()) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache needs Jib's default entrypoint, which starts "
                + "java with the application class path. Remove the Jib container.entrypoint configuration.");
        }
        String containerizingMode = mavenSession.getUserProperties().getProperty(PropertyNames.CONTAINERIZING_MODE);
        if (containerizingMode != null && !PACKAGED.equals(containerizingMode)) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache needs jib.containerizingMode=" + PACKAGED
                + ", because a JDK AOT cache cannot use classes from a directory, but it is set to " + containerizingMode);
        }
        if (jdkAotCacheTrainingTimeout <= 0) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache.trainingTimeout must be positive");
        }
        return JdkAotCacheTraining.validateTrainingPaths(jdkAotCacheTrainingPaths);
    }

    private String daemonPlatform() throws MojoExecutionException {
        try {
            return dockerService.getDaemonPlatform();
        } catch (IllegalStateException e) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache runs the application in a Docker container to "
                + "train the cache, so it needs a Docker daemon, also with jib.buildGoal=buildTar or build. "
                + e.getMessage(), e);
        }
    }

    private void requireDaemonPlatform(String platform) throws MojoExecutionException {
        Set<JibConfiguration.PlatformConfiguration> platforms = jibConfigurationService.getFromPlatforms();
        if (platforms.isEmpty()) {
            return;
        }
        List<String> configured = platforms.stream()
            .map(p -> p.os().orElse("linux") + "/" + DockerService.goArchitecture(p.architecture().orElse("amd64")))
            .distinct()
            .toList();
        if (!configured.equals(List.of(platform))) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache trains the cache on the Docker daemon, whose "
                + "platform is " + platform + ", and a cache only works on the platform that trained it. The Jib "
                + "from.platforms configuration lists " + configured + ": remove it, or set it to " + platform + " only.");
        }
    }

    private boolean isRegistryBaseImage() {
        String baseImage = Optional.ofNullable(jibProperty(PropertyNames.FROM_IMAGE))
            .or(jibConfigurationService::getFromImage)
            .orElse("");
        return !baseImage.startsWith(DOCKER_URI_PREFIX) && !baseImage.startsWith(TAR_URI_PREFIX);
    }

    /**
     * @return the image that Jib builds, with the precedence of the Jib Maven plugin
     */
    private String targetImage() {
        return Optional.ofNullable(jibProperty(PropertyNames.TO_IMAGE_ALTERNATE))
            .or(() -> Optional.ofNullable(jibProperty(PropertyNames.TO_IMAGE)))
            .orElseGet(() -> getTags().iterator().next());
    }

    /**
     * Resolves a Jib property as the Jib Maven plugin does: user properties, project properties, then system
     * properties.
     */
    private String jibProperty(String name) {
        for (Properties properties : List.of(mavenSession.getUserProperties(), mavenProject.getProperties(), mavenSession.getSystemProperties())) {
            if (properties.containsKey(name)) {
                return properties.getProperty(name);
            }
        }
        return null;
    }

    private void verifyFinalImageLayers(String trainingImageId, String finalImage) throws MojoExecutionException {
        List<String> trainingLayers = inspectImage(trainingImageId).map(DockerMojo::layers).orElse(List.of());
        List<String> finalLayers = inspectImage(finalImage).map(DockerMojo::layers).orElse(List.of());
        if (trainingLayers.isEmpty() || finalLayers.isEmpty()) {
            getLog().warn("JDK AOT cache: could not compare the layers of " + finalImage + " with the training image");
            return;
        }
        if (finalLayers.size() != trainingLayers.size() + 1 || !finalLayers.subList(0, trainingLayers.size()).equals(trainingLayers)) {
            throw new MojoExecutionException("JDK AOT cache: the layers of " + finalImage + " differ from the layers "
                + "of the training image, so the cache may not match the image. Did the base image change during the "
                + "build? Build the image again.");
        }
        getLog().info("JDK AOT cache: " + finalImage + " has the " + trainingLayers.size()
            + " layers of the training image and the cache layer");
    }

    private Optional<InspectImageResponse> inspectImage(String image) {
        try {
            return Optional.of(dockerService.inspectImage(image));
        } catch (NotFoundException e) {
            return Optional.empty();
        }
    }

    private static List<String> layers(InspectImageResponse image) {
        return image.getRootFS() == null || image.getRootFS().getLayers() == null ? List.of() : image.getRootFS().getLayers();
    }

    private void removeTrainingImage(String trainingImageId) {
        try {
            dockerService.removeImage(trainingImageId);
        } catch (RuntimeException e) {
            getLog().warn("JDK AOT cache: could not remove the training image " + trainingImageId + ": " + e.getMessage());
        }
    }

    private static Map<String, String> saveProperties(Properties properties, String... names) {
        var saved = new HashMap<String, String>();
        for (String name : names) {
            saved.put(name, properties.getProperty(name));
        }
        return saved;
    }

    private static void restoreProperties(Properties properties, Map<String, String> saved) {
        saved.forEach((name, value) -> restoreProperty(properties, name, value));
    }

    private static void restoreProperty(Properties properties, String name, String value) {
        if (value == null) {
            properties.remove(name);
        } else {
            properties.setProperty(name, value);
        }
    }

    private File determineDockerfile(File providedDockerfile) throws MojoExecutionException {
        if (providedDockerfile.exists()) {
            return providedDockerfile;
        }
        try {
            return dockerService.loadDockerfileAsResource(DOCKERFILE_ORACLE_CLOUD);
        } catch (IOException e) {
            throw new MojoExecutionException("Error loading Dockerfile", e);
        }
    }

    private void buildDockerfile(File dockerfile, boolean providedDockerfileExists) throws MojoExecutionException {
        try {
            var runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());
            if (runtime.getBuildStrategy() == DockerBuildStrategy.ORACLE_FUNCTION && !providedDockerfileExists) {
                oracleCloudFunctionCmd(dockerfile);
                DockerfileMojo.processOracleFunctionDockerfile(dockerfile);
            }
            getLog().info("Using Dockerfile: " + dockerfile.getAbsolutePath());
            mavenProject.getProperties().put(PropertyNames.SKIP, "true");

            copyDependencies();

            String targetDir = mavenProject.getBuild().getDirectory();
            var targetDockerfile = new File(targetDir, dockerfile.getName());
            Files.copy(dockerfile.toPath(), targetDockerfile.toPath(), LinkOption.NOFOLLOW_LINKS,
                StandardCopyOption.REPLACE_EXISTING);

            BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                .withDockerfile(targetDockerfile)
                .withTags(getTags())
                .withBaseDirectory(new File(targetDir));
            getNetworkMode().ifPresent(buildImageCmd::withNetworkMode);
            dockerService.buildImage(buildImageCmd);
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

}
