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

import com.google.cloud.tools.jib.api.CacheDirectoryCreationException;
import com.google.cloud.tools.jib.api.Containerizer;
import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.cloud.tools.jib.api.Jib;
import com.google.cloud.tools.jib.api.JibContainer;
import com.google.cloud.tools.jib.api.JibContainerBuilder;
import com.google.cloud.tools.jib.api.LogEvent;
import com.google.cloud.tools.jib.api.RegistryException;
import com.google.cloud.tools.jib.api.RegistryImage;
import com.google.cloud.tools.jib.api.TarImage;
import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.ImageFormat;
import com.google.cloud.tools.jib.api.buildplan.Platform;
import com.google.cloud.tools.jib.api.buildplan.Port;
import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jib.JibConfiguration;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ExecutionException;

/**
 * Builds a container image from a locally compiled native executable with Jib Core.
 */
@Mojo(name = NativeImageJibMojo.NATIVE_IMAGE_JIB_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class NativeImageJibMojo extends AbstractDockerMojo {

    public static final String NATIVE_IMAGE_JIB_PACKAGING = "native-image-jib";
    static final String EXECUTABLE_PROPERTY = "micronaut.native-image.jib.executable";
    static final String BASE_IMAGE_PROPERTY = "micronaut.native-image.jib.base-image";
    static final String ALLOW_PLATFORM_MISMATCH_PROPERTY = "micronaut.native-image.jib.allow-platform-mismatch";
    static final String DEFAULT_APP_ROOT = "/app";
    static final String DEFAULT_TAR_NAME = "jib-image.tar";
    private static final String JIB_BUILD_GOAL_PROPERTY = "jib.buildGoal";
    private static final String JIB_BUILD_GOAL_PARAMETER = "jibBuildGoal";
    private static final String JIB_BUILD_GOAL_EXPRESSION = "${" + JIB_BUILD_GOAL_PROPERTY + "}";
    private static final String DEFAULT_JIB_BUILD_GOAL = "buildTar";
    private static final String INHERITED_DOCKER_BUILD_GOAL = "dockerBuild";
    private static final Logger LOG = LoggerFactory.getLogger(NativeImageJibMojo.class);
    private static final List<String> SUPPORTED_JIB_BUILD_GOALS = List.of(DEFAULT_JIB_BUILD_GOAL, "build");
    private static final String GRAALVM_NATIVE_PLUGIN_KEY = "org.graalvm.buildtools:native-maven-plugin";
    private static final String DEFAULT_USER = "65532";
    private static final String LINUX = "linux";
    private static final String AMD64 = "amd64";
    private static final String ARM64 = "arm64";

    /**
     * Native executable to copy into the image. Defaults to {@code target/<native imageName or artifactId>}.
     */
    @Parameter(property = EXECUTABLE_PROPERTY)
    protected File executable;

    /**
     * Runtime base image for the native executable image. Jib {@code from.image} has higher precedence.
     */
    @Parameter(property = BASE_IMAGE_PROPERTY)
    protected String nativeImageJibBaseImage;

    /**
     * Allows packaging when the host OS/architecture differs from the selected Linux container platform.
     */
    @Parameter(property = ALLOW_PLATFORM_MISMATCH_PROPERTY, defaultValue = "false")
    protected boolean allowPlatformMismatch;

    private final String pluginVersion;
    private final MojoExecution mojoExecution;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public NativeImageJibMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                              ApplicationConfigurationService applicationConfigurationService, DockerService dockerService,
                              MavenSession mavenSession, MojoExecution mojoExecution) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession,
            mojoExecution);
        this.mojoExecution = mojoExecution;
        pluginVersion = Optional.ofNullable(mojoExecution)
            .map(MojoExecution::getPlugin)
            .map(Plugin::getVersion)
            .filter(StringUtils::hasText)
            .orElse("unknown");
    }

    @Override
    public void execute() throws MojoExecutionException {
        applyDefaultJibBuildGoal();
        validateJibBuildGoal();
        validateRuntime();
        Platform platform = resolvePlatform();
        validatePlatform(platform);
        Path nativeExecutable = resolveExecutable();
        if (!Files.isRegularFile(nativeExecutable)) {
            throw new MojoExecutionException("Native executable not found: " + nativeExecutable
                + ". Build with native-image packaging first, or set -D" + EXECUTABLE_PROPERTY + "=<path>.");
        }
        ImageReference imageReference = parseImageReference(primaryImage());
        JibContainerBuilder builder = createContainerBuilder(nativeExecutable, platform);
        Containerizer containerizer = createContainerizer(imageReference);
        try {
            JibContainer jibContainer = containerize(builder, containerizer);
            getLog().info("Built native image container " + jibContainer.getTargetImage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new MojoExecutionException("Interrupted while building native image container", e);
        } catch (CacheDirectoryCreationException | ExecutionException | IOException | RegistryException e) {
            throw new MojoExecutionException("Failed to build native image container with Jib: " + e.getMessage(), e);
        }
    }

    final JibContainerBuilder createContainerBuilder(Path nativeExecutable, Platform platform) throws MojoExecutionException {
        String baseImage = baseImage();
        String executableName = nativeExecutable.getFileName().toString();
        AbsoluteUnixPath containerExecutable = AbsoluteUnixPath.get(DEFAULT_APP_ROOT + "/" + executableName);
        var executableLayer = FileEntriesLayer.builder()
            .setName("native executable")
            .addEntry(nativeExecutable, containerExecutable, FilePermissions.fromOctalString("755"))
            .build();
        return fromBaseImage(baseImage)
            .addFileEntriesLayer(executableLayer)
            .setEntrypoint(entrypoint(containerExecutable))
            .setProgramArguments(programArguments())
            .setExposedPorts(exposedPorts())
            .setFormat(ImageFormat.OCI)
            .setPlatforms(Set.of(platform))
            .setWorkingDirectory(AbsoluteUnixPath.get(DEFAULT_APP_ROOT))
            .setUser(jibConfigurationService.getUser().orElse(DEFAULT_USER));
    }

    private JibContainerBuilder fromBaseImage(String baseImage) throws MojoExecutionException {
        if ("scratch".equals(baseImage)) {
            return Jib.fromScratch();
        }
        RegistryImage fromImage = registryImage(baseImage, jibConfigurationService.getFromCredentials());
        if (jibConfigurationService.getFromCredentials().isEmpty()) {
            jibConfigurationService.resolveCredentialForImage(baseImage, LOG)
                .ifPresent(credential -> addCredential(fromImage, credential));
        }
        return Jib.from(fromImage);
    }

    final JibContainer containerize(JibContainerBuilder builder, Containerizer containerizer)
        throws InterruptedException, RegistryException, IOException, CacheDirectoryCreationException, ExecutionException {
        return builder.containerize(containerizer);
    }

    private Containerizer createContainerizer(ImageReference imageReference) throws MojoExecutionException {
        Containerizer containerizer;
        if (DEFAULT_JIB_BUILD_GOAL.equals(jibBuildGoal)) {
            Path output = jibConfigurationService.getOutputPathsTar()
                .map(Path::of)
                .orElseGet(() -> Path.of(mavenProject.getBuild().getDirectory(), DEFAULT_TAR_NAME));
            containerizer = Containerizer.to(TarImage.at(output).named(imageReference));
        } else {
            RegistryImage targetImage = registryImage(imageReference.toString(), jibConfigurationService.getToCredentials());
            if (jibConfigurationService.getToCredentials().isEmpty()) {
                jibConfigurationService.resolveCredentialForImage(imageReference.toString(), LOG)
                    .ifPresent(credential -> addCredential(targetImage, credential));
            }
            containerizer = Containerizer.to(targetImage);
        }
        for (String tag : additionalTags(imageReference)) {
            containerizer.withAdditionalTag(tag);
        }
        return containerizer
            .addEventHandler(LogEvent.class, this::logJibEvent)
            .setToolName("micronaut-maven-plugin")
            .setToolVersion(pluginVersion);
    }

    private void validateJibBuildGoal() throws MojoExecutionException {
        if (!SUPPORTED_JIB_BUILD_GOALS.contains(jibBuildGoal)) {
            throw new MojoExecutionException("Unsupported jib.buildGoal '" + jibBuildGoal
                + "' for native-image-jib packaging. Supported values are: " + String.join(", ", SUPPORTED_JIB_BUILD_GOALS)
                + ". Use docker-native packaging for Docker-backed native image builds.");
        }
    }

    private void applyDefaultJibBuildGoal() {
        if (INHERITED_DOCKER_BUILD_GOAL.equals(jibBuildGoal) && !hasConfiguredJibBuildGoal()) {
            jibBuildGoal = DEFAULT_JIB_BUILD_GOAL;
        }
    }

    private boolean hasConfiguredJibBuildGoal() {
        return hasProperty(mavenSession.getUserProperties(), JIB_BUILD_GOAL_PROPERTY)
            || hasProperty(mavenSession.getSystemProperties(), JIB_BUILD_GOAL_PROPERTY)
            || hasProperty(mavenProject.getProperties(), JIB_BUILD_GOAL_PROPERTY)
            || hasConfiguredMojoParameter(JIB_BUILD_GOAL_PARAMETER);
    }

    private static boolean hasProperty(Properties properties, String key) {
        return properties != null && properties.containsKey(key);
    }

    private boolean hasConfiguredMojoParameter(String parameterName) {
        if (mojoExecution == null || !(mojoExecution.getConfiguration() instanceof Xpp3Dom configuration)) {
            return false;
        }
        Xpp3Dom parameter = configuration.getChild(parameterName);
        return parameter != null && !isDescriptorDefaultJibBuildGoal(parameter);
    }

    private static boolean isDescriptorDefaultJibBuildGoal(Xpp3Dom parameter) {
        return JIB_BUILD_GOAL_EXPRESSION.equals(parameter.getValue())
            && INHERITED_DOCKER_BUILD_GOAL.equals(parameter.getAttribute("default-value"));
    }

    private void validateRuntime() throws MojoExecutionException {
        MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase(Locale.ENGLISH));
        DockerBuildStrategy buildStrategy = runtime.getBuildStrategy();
        if (buildStrategy == DockerBuildStrategy.LAMBDA || buildStrategy == DockerBuildStrategy.ORACLE_FUNCTION) {
            throw new MojoExecutionException("native-image-jib packaging does not support micronaut.runtime="
                + micronautRuntime + ". Use docker-native packaging for Lambda and Oracle Function native images.");
        }
    }

    private Path resolveExecutable() {
        if (executable != null) {
            return executable.toPath();
        }
        return Path.of(mavenProject.getBuild().getDirectory(), nativeImageName());
    }

    private String nativeImageName() {
        return configuredNativeImageName().orElse(mavenProject.getArtifactId());
    }

    private Optional<String> configuredNativeImageName() {
        Plugin plugin = mavenProject.getPlugin(GRAALVM_NATIVE_PLUGIN_KEY);
        if (plugin != null && plugin.getConfiguration() instanceof Xpp3Dom configuration) {
            Xpp3Dom imageName = configuration.getChild("imageName");
            if (imageName != null && StringUtils.hasText(imageName.getValue())) {
                return Optional.of(evaluateMavenExpression(imageName.getValue()));
            }
        }
        return Optional.empty();
    }

    private String baseImage() throws MojoExecutionException {
        String image = getJibFromImageSystemProperty()
            .or(() -> getFromImage().filter(StringUtils::hasText))
            .or(() -> Optional.ofNullable(nativeImageJibBaseImage).filter(StringUtils::hasText))
            .or(() -> Optional.ofNullable(baseImageRun).filter(StringUtils::hasText))
            .orElse(DEFAULT_BASE_IMAGE_GRAALVM_RUN);
        return validateImageReference("native-image-jib base image", evaluateMavenExpression(image));
    }

    private ImageReference parseImageReference(String image) throws MojoExecutionException {
        try {
            return ImageReference.parse(image);
        } catch (InvalidImageReferenceException e) {
            throw new MojoExecutionException("native-image-jib target image is not a valid image reference: " + image, e);
        }
    }

    private RegistryImage registryImage(String image, Optional<Credential> credential) throws MojoExecutionException {
        try {
            RegistryImage registryImage = RegistryImage.named(image);
            credential.ifPresent(value -> addCredential(registryImage, value));
            return registryImage;
        } catch (InvalidImageReferenceException e) {
            throw new MojoExecutionException("Invalid image reference for native-image-jib: " + image, e);
        }
    }

    private static void addCredential(RegistryImage image, Credential credential) {
        image.addCredential(credential.getUsername(), credential.getPassword());
    }

    private String primaryImage() {
        String image = jibConfigurationService.getToImage()
            .map(this::evaluateMavenExpression)
            .filter(StringUtils::hasText)
            .orElse(mavenProject.getArtifactId());
        return ensureTag(image);
    }

    private Set<String> additionalTags(ImageReference primaryImage) throws MojoExecutionException {
        Set<String> tags = new LinkedHashSet<>();
        String primaryTag = primaryImage.getTag().orElse(LATEST_TAG);
        for (String tag : jibConfigurationService.getTags()) {
            String evaluated = evaluateMavenExpression(tag);
            if (!StringUtils.hasText(evaluated) || evaluated.equals(primaryTag)) {
                continue;
            }
            if (!ImageReference.isValidTag(evaluated)) {
                throw new MojoExecutionException("jib.to.tags contains an invalid image tag for native-image-jib: " + evaluated);
            }
            tags.add(evaluated);
        }
        return tags;
    }

    private static String ensureTag(String image) {
        int lastSlash = image.lastIndexOf('/');
        int tagSeparator = image.indexOf(':', lastSlash + 1);
        if (tagSeparator < 0 && !image.contains("@")) {
            return image + ":" + LATEST_TAG;
        }
        return image;
    }

    private List<String> entrypoint(AbsoluteUnixPath containerExecutable) {
        List<String> configuredEntrypoint = jibConfigurationService.getEntrypoint();
        if (!configuredEntrypoint.isEmpty()) {
            return configuredEntrypoint;
        }
        return List.of(containerExecutable.toString());
    }

    private List<String> programArguments() {
        List<String> args = jibConfigurationService.getArgs();
        if (!args.isEmpty()) {
            return args;
        }
        if (appArguments != null) {
            return appArguments;
        }
        return List.of();
    }

    private Set<Port> exposedPorts() throws MojoExecutionException {
        String ports = validateExposedPorts("jib.container.ports", getPorts());
        if (!StringUtils.hasText(ports)) {
            return Set.of();
        }
        try {
            return com.google.cloud.tools.jib.api.Ports.parse(List.of(ports.trim().split("\\s+")));
        } catch (IllegalArgumentException e) {
            throw new MojoExecutionException("native-image-jib supports individual exposed ports such as 8080 or 8080/tcp: " + ports, e);
        }
    }

    private Platform resolvePlatform() throws MojoExecutionException {
        Set<JibConfiguration.PlatformConfiguration> configuredPlatforms = jibConfigurationService.getFromPlatforms();
        if (configuredPlatforms.isEmpty()) {
            return detectedPlatform();
        }
        if (configuredPlatforms.size() > 1) {
            throw new MojoExecutionException("native-image-jib supports exactly one target platform because it packages one local native executable.");
        }
        JibConfiguration.PlatformConfiguration configuredPlatform = configuredPlatforms.iterator().next();
        String os = configuredPlatform.os().orElse(LINUX);
        String architecture = configuredPlatform.architecture()
            .map(NativeImageJibMojo::normalizeArchitecture)
            .orElseThrow(() -> new MojoExecutionException("jib.from.platforms must define an architecture for native-image-jib packaging."));
        return new Platform(architecture, os);
    }

    private Platform detectedPlatform() {
        return new Platform(normalizeArchitecture(System.getProperty("os.arch")), LINUX);
    }

    private void validatePlatform(Platform platform) throws MojoExecutionException {
        if (!LINUX.equals(platform.getOs())) {
            throw new MojoExecutionException("native-image-jib packages Linux container images only. Configured platform is "
                + platform.getOs() + "/" + platform.getArchitecture() + ".");
        }
        if (allowPlatformMismatch) {
            return;
        }
        String hostOs = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
        if (!hostOs.contains(LINUX)) {
            throw new MojoExecutionException("native-image-jib requires a Linux host by default because the local native executable is copied into a Linux container image. "
                + "Use docker-native packaging for Docker-backed cross-platform builds, or set -D" + ALLOW_PLATFORM_MISMATCH_PROPERTY + "=true for a known Linux cross-compiled executable.");
        }
        String hostArchitecture = normalizeArchitecture(System.getProperty("os.arch"));
        if (!hostArchitecture.equals(platform.getArchitecture())) {
            throw new MojoExecutionException("native-image-jib host architecture " + hostArchitecture
                + " does not match configured target architecture " + platform.getArchitecture()
                + ". Set -D" + ALLOW_PLATFORM_MISMATCH_PROPERTY + "=true only for a known compatible cross-compiled executable.");
        }
    }

    private static String normalizeArchitecture(String architecture) {
        return switch (architecture) {
            case "x86_64", "x64", AMD64 -> AMD64;
            case "aarch64", ARM64 -> ARM64;
            default -> architecture;
        };
    }

    private String evaluateMavenExpression(String expression) {
        try {
            return expressionEvaluator.evaluate(expression, String.class).toString();
        } catch (Exception e) {
            LOG.debug("Could not evaluate Maven expression '{}'", expression, e);
            return expression;
        }
    }

    private void logJibEvent(LogEvent event) {
        switch (event.getLevel()) {
            case ERROR -> getLog().error(event.getMessage());
            case WARN -> getLog().warn(event.getMessage());
            case DEBUG -> getLog().debug(event.getMessage());
            default -> getLog().info(event.getMessage());
        }
    }
}
