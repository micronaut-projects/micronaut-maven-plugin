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

import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import com.google.common.io.FileWriteMode;
import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.jib.JibMicronautExtension;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static io.micronaut.maven.services.ApplicationConfigurationService.DEFAULT_PORT;

/**
 * Abstract base class for mojos related to Docker files and builds.
 *
 * @author Álvaro Sánchez-Mariscal
 * @author Iván López
 * @since 1.1
 */
public abstract class AbstractDockerMojo extends AbstractMicronautMojo {

    public static final String LATEST_TAG = "latest";
    public static final String DEFAULT_BASE_IMAGE_GRAALVM_RUN = "cgr.dev/chainguard/wolfi-base@sha256:a5a619c1793039dcf92f02178f37c94bb3d6001403716da59d6092dfe8d9b502";
    public static final String DEFAULT_BASE_IMAGE_GRAALVM_BUILD = "container-registry.oracle.com/graalvm/native-image";
    public static final String MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG = "-H:+StaticExecutableWithDynamicLibC";
    public static final String ARM_ARCH = "aarch64";
    public static final String X86_64_ARCH = "x64";
    public static final String ORACLE_CLOUD_FUNCTION_DEFAULT_CMD = "CMD [\"io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest\"]";
    public static final String GDS_DOWNLOAD_URL = "https://gds.oracle.com/download/graal/%s/latest-gftc/graalvm-jdk-%s_linux-%s_bin.tar.gz";
    public static final String LAMBDA_BOOTSTRAP_DOCKER_COMMAND_PLACEHOLDER = "${LAMBDA_BOOTSTRAP_DOCKER_COMMAND}";
    static final String JIB_FROM_IMAGE_PROPERTY = "jib.from.image";
    private static final String DEPENDENCY_DIRECTORY = "dependency";
    private static final String RELEASE_DEPENDENCY_DIRECTORY = "release";
    private static final String SNAPSHOT_DEPENDENCY_DIRECTORY = "snapshot";
    private static final NavigableSet<Integer> GRAALVM_VERSIONS = new TreeSet<>(Set.of(25));
    private static final List<String> DEFAULT_LAMBDA_BOOTSTRAP_ARGUMENTS = List.of(
        "-XX:MaximumHeapSizePercent=80",
        "-Dio.netty.allocator.numDirectArenas=0",
        "-Dio.netty.noPreferDirect=true",
        "-Djava.library.path=$(pwd)"
    );

    protected final MavenProject mavenProject;
    protected final MavenSession mavenSession;
    protected final JibConfigurationService jibConfigurationService;
    protected final ApplicationConfigurationService applicationConfigurationService;
    protected final DockerService dockerService;
    protected final PluginParameterExpressionEvaluator expressionEvaluator;


    /**
     * Additional arguments that will be passed to the <code>native-image</code> executable. Note that this will only
     * be used when using a packaging of type <code>docker-native</code>. For <code>native-image</code> packaging
     * you should use the
     * <a href="https://www.graalvm.org/reference-manual/native-image/NativeImageMavenPlugin/#maven-plugin-customization">
     * Native Image Maven Plugin
     * </a> configuration options.
     */
    @Parameter(property = "micronaut.native-image.args")
    protected List<String> nativeImageBuildArgs;

    /**
     * List of additional arguments that will be passed to the application.
     */
    @Parameter(property = RunMojo.MN_APP_ARGS)
    protected List<String> appArguments;

    /**
     * Additional arguments that will be appended to the generated AWS Lambda native bootstrap command.
     *
     * @since 5.0.0
     */
    @Parameter(property = "micronaut.lambda.bootstrap.args")
    protected List<String> lambdaBootstrapArguments;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = RunMojo.EXEC_MAIN_CLASS, required = true)
    protected String mainClass;

    /**
     * Whether to produce a static native image when using <code>docker-native</code> packaging.
     */
    @Parameter(defaultValue = "false", property = "micronaut.native-image.static")
    protected Boolean staticNativeImage;

    /**
     * The target runtime of the application.
     */
    @Parameter(property = MicronautRuntime.PROPERTY, defaultValue = "NONE")
    protected String micronautRuntime;

    /**
     * The Docker image used to run the native image.
     *
     * @since 1.2
     */
    @Parameter(property = "micronaut.native-image.base-image-run", defaultValue = DEFAULT_BASE_IMAGE_GRAALVM_RUN)
    protected String baseImageRun;

    /**
     * The builder-stage base image used to build the native image for docker-native packaging variants.
     *
     * @since 5.0.0
     */
    @Parameter(property = "micronaut.native-image.base-image")
    protected String baseImage;

    /**
     * The version of Oracle Linux to use as a native-compile base when building a native image inside a Docker container.
     */
    @Parameter(property = "micronaut.native-image.ol.version", defaultValue = "ol9")
    protected String oracleLinuxVersion;

    /**
     * Networking mode for the RUN instructions during build.
     *
     * @since 4.0.0
     */
    @Parameter(property = "docker.networkMode")
    protected String networkMode;

    /**
     * <p>
     * Jib goal used to build Docker images for {@code docker} packaging.
     * </p>
     * <p>
     * Defaults to {@code dockerBuild}. Set it to {@code buildTar} or {@code build} to avoid talking to a local Docker daemon during {@code package}.
     * </p>
     *
     * @since 5.0.0
     */
    @Parameter(property = "jib.buildGoal", defaultValue = "dockerBuild")
    protected String jibBuildGoal;

    protected AbstractDockerMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                                 ApplicationConfigurationService applicationConfigurationService,
                                 DockerService dockerService, MavenSession mavenSession, MojoExecution mojoExecution) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.jibConfigurationService = jibConfigurationService;
        this.applicationConfigurationService = applicationConfigurationService;
        this.dockerService = dockerService;
        this.expressionEvaluator = new PluginParameterExpressionEvaluator(mavenSession, mojoExecution);
    }

    /**
     * @return the Java version from either the <code>maven.compiler.target</code> property or the <code>java.version</code> property.
     */
    protected ArtifactVersion javaVersion() {
        return new DefaultArtifactVersion(getJdkVersion());
    }

    private String getJdkVersion() {
        var releaseVersion = getPropertyValue(mavenProject, "maven.compiler.release");
        var targetVersion = getPropertyValue(mavenProject, "maven.compiler.target");
        return releaseVersion.or(() -> targetVersion).orElseGet(() -> System.getProperty("java.version"));
    }

    private static Optional<String> getPropertyValue(MavenProject project, String propertName) {
        var systemProperty = Optional.of(propertName).map(System::getProperty);
        var properties = project.getProperties();
        var projectProperty = Optional.of(propertName).map(properties::getProperty);
        return systemProperty.or(() -> projectProperty);
    }

    /**
     * @return the JVM version to use for GraalVM.
     */
    protected String graalVmJvmVersion() {
        return Integer.toString(resolveGraalVersion());
    }

    /**
     * @return the GraalVM download URL depending on the Java version.
     */
    protected String graalVmDownloadUrl() {
        Integer version = resolveGraalVersion();

        return GDS_DOWNLOAD_URL.formatted(version, version, graalVmArch());
    }

    private Integer resolveGraalVersion() {
        int target = javaVersion().getMajorVersion();
        Integer version = GRAALVM_VERSIONS.floor(target);

        return version != null ? version : GRAALVM_VERSIONS.first();
    }

    /**
     * @return the OS architecture to use for GraalVM depending on the <code>os.arch</code> system property.
     */
    protected String graalVmArch() {
        return isArm() ? ARM_ARCH : X86_64_ARCH;
    }

    /**
     * @return the base FROM image for the native image.
     */
    protected String getFrom() {
        return getJibFromImageSystemProperty()
            .or(() -> Optional.ofNullable(baseImage).filter(StringUtils::hasText))
            .or(() -> getFromImage().filter(StringUtils::hasText))
            .orElse(DEFAULT_BASE_IMAGE_GRAALVM_BUILD + ":" + graalVmTag(graalVmJvmVersion(), staticNativeImage, oracleLinuxVersion));
    }

    /**
     * @param graalVmJvmVersion the JVM version string
     * @param staticNativeImage whether to produce a static native image
     * @param oracleLinuxVersion the Oracle Linux version to use
     * @return the GraalVM Docker image tag based on the provided parameters
     */
    protected String graalVmTag(String graalVmJvmVersion, Boolean staticNativeImage, String oracleLinuxVersion) {
        String suffix = Boolean.TRUE.equals(staticNativeImage)
            ? "-muslib" + (StringUtils.hasText(oracleLinuxVersion) ? "-" + oracleLinuxVersion : "")
            : (StringUtils.hasText(oracleLinuxVersion) ? "-" + oracleLinuxVersion : "");
        return graalVmJvmVersion + suffix;
    }

    /**
     * Check os.arch against known ARM architecture identifiers.
     *
     * @return true if we think we're running on an arm JDK
     */
    protected boolean isArm() {
        return switch (System.getProperty("os.arch")) {
            case ARM_ARCH, "arm64" -> true;
            default -> false;
        };
    }

    /**
     * @return the base image from the jib configuration (if any).
     */
    protected Optional<String> getFromImage() {
        return jibConfigurationService.getFromImage();
    }

    /**
     * @return the base image from the Jib system property override, if any.
     */
    protected Optional<String> getJibFromImageSystemProperty() {
        return Optional.ofNullable(System.getProperty(JIB_FROM_IMAGE_PROPERTY))
            .filter(StringUtils::hasText);
    }

    /**
     * @return the Docker image tags by looking at the Jib plugin configuration.
     */
    protected Set<String> getTags() {
        var tags = new HashSet<String>();
        Optional<String> toImageOptional = jibConfigurationService.getToImage();
        String imageName = mavenProject.getArtifactId();
        if (toImageOptional.isPresent()) {
            String toImage = toImageOptional.get();
            if (toImage.contains(":")) {
                tags.add(toImage);
                imageName = toImageOptional.get().split(":")[0];
            } else {
                tags.add(toImage + ":" + LATEST_TAG);
                imageName = toImage;
            }
        } else {
            tags.add(imageName + ":" + LATEST_TAG);
        }
        for (String tag : jibConfigurationService.getTags()) {
            if (LATEST_TAG.equals(tag) && tags.stream().anyMatch(t -> t.contains(LATEST_TAG))) {
                continue;
            }
            tags.add(String.format("%s:%s", imageName, tag));
        }
        return tags.stream()
            .map(this::evaluateExpression)
            .collect(Collectors.toSet());
    }

    private String evaluateExpression(String expression) {
        try {
            return expressionEvaluator.evaluate(expression, String.class).toString();
        } catch (Exception e) {
            return expression;
        }
    }

    /**
     * @return the application ports to expose by looking at the Jib configuration or the application configuration.
     */
    protected String getPorts() {
        return jibConfigurationService.getPorts().orElseGet(() -> {
            String port = applicationConfigurationService.getServerPort();
            return "-1".equals(port) ? DEFAULT_PORT : port;
        });
    }

    /**
     * Copy project dependencies to a <code>target/dependency</code> directory.
     */
    protected void copyDependencies() throws IOException {
        var imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        var target = new File(mavenProject.getBuild().getDirectory(), DEPENDENCY_DIRECTORY).toPath();
        Files.createDirectories(target);
        Files.createDirectories(target.resolve(RELEASE_DEPENDENCY_DIRECTORY));
        Files.createDirectories(target.resolve(SNAPSHOT_DEPENDENCY_DIRECTORY));
        for (Artifact dependency : mavenProject.getArtifacts()) {
            if (!imageClasspathScopes.contains(dependency.getScope())) {
                continue;
            }
            var dependencyFile = dependency.getFile().toPath();
            var dependencyName = dependency.getFile().getName();
            var layeredPath = target.resolve(dependencyLayerDirectory(dependency)).resolve(dependencyName);
            Files.copy(dependencyFile, layeredPath, StandardCopyOption.REPLACE_EXISTING);
            Files.copy(dependencyFile, target.resolve(dependencyName), StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static String dependencyLayerDirectory(Artifact dependency) {
        return dependency.isSnapshot() ? SNAPSHOT_DEPENDENCY_DIRECTORY : RELEASE_DEPENDENCY_DIRECTORY;
    }

    /**
     * @return the Docker CMD command.
     */
    protected String getCmd() throws MojoExecutionException {
        var escapedArguments = new ArrayList<String>(appArguments.size());
        for (String argument : appArguments) {
            escapedArguments.add(jsonStringLiteral("mn.app.args", argument));
        }
        return "CMD [" + String.join(", ", escapedArguments) + "]";
    }

    /**
     * @return the generated AWS Lambda bootstrap command.
     */
    protected String getLambdaBootstrapCommand() throws MojoExecutionException {
        var command = new StringBuilder("./func");
        for (String bootstrapArgument : DEFAULT_LAMBDA_BOOTSTRAP_ARGUMENTS) {
            command.append(' ').append(bootstrapArgument);
        }
        if (lambdaBootstrapArguments != null && !lambdaBootstrapArguments.isEmpty()) {
            for (String lambdaBootstrapArgument : lambdaBootstrapArguments) {
                command.append(' ').append(escapeBootstrapArgument(lambdaBootstrapArgument));
            }
        }
        return command.toString();
    }

    /**
     * Applies the generated AWS Lambda bootstrap script to a dockerfile template.
     *
     * @param dockerfile the docker file
     */
    protected void lambdaBootstrapCommand(File dockerfile) throws IOException, MojoExecutionException {
        if (dockerfile == null) {
            return;
        }
        String lambdaBootstrapDockerCommand = getLambdaBootstrapDockerCommand();
        if (lambdaBootstrapArguments != null && !lambdaBootstrapArguments.isEmpty()) {
            getLog().info("Using AWS Lambda bootstrap arguments: " + lambdaBootstrapArguments);
        }
        var allLines = Files.readAllLines(dockerfile.toPath());
        var result = new ArrayList<String>(allLines.size());
        for (String line : allLines) {
            if (line.contains(LAMBDA_BOOTSTRAP_DOCKER_COMMAND_PLACEHOLDER)) {
                result.add(line.replace(LAMBDA_BOOTSTRAP_DOCKER_COMMAND_PLACEHOLDER, lambdaBootstrapDockerCommand));
            } else {
                result.add(line);
            }
        }
        Files.write(dockerfile.toPath(), result);
    }

    private String getLambdaBootstrapDockerCommand() throws MojoExecutionException {
        var quotedLines = new ArrayList<String>();
        for (String line : List.of("#!/bin/sh", "set -euo pipefail", getLambdaBootstrapCommand())) {
            quotedLines.add(quoteShellLiteral("AWS Lambda bootstrap command", line));
        }
        return "printf '%s\\n' " + String.join(" ", quotedLines) + " > bootstrap";
    }

    private static String escapeBootstrapArgument(String argument) throws MojoExecutionException {
        String sanitized = validateDockerfileValue("micronaut.lambda.bootstrap.args", argument);
        if (isShellSafe(sanitized)) {
            return sanitized;
        }
        return quoteShellLiteral("micronaut.lambda.bootstrap.args", sanitized);
    }

    private static boolean isShellSafe(String argument) {
        if (argument == null || argument.isEmpty()) {
            return false;
        }
        for (int i = 0; i < argument.length(); i++) {
            char c = argument.charAt(i);
            if (!Character.isLetterOrDigit(c)
                && "_@%+=:,./-".indexOf(c) == -1) {
                return false;
            }
        }
        return true;
    }

    static String quoteShellLiteral(String source, String value) throws MojoExecutionException {
        validateDockerfileValue(source, value);
        return "'" + value.replace("'", "'\"'\"'") + "'";
    }

    protected static String validateDockerfileValue(String source, String value) throws MojoExecutionException {
        if (value == null) {
            throw new MojoExecutionException(source + " must not be null when generating a Dockerfile");
        }
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (Character.isISOControl(c)) {
                throw new MojoExecutionException(source + " contains an unsupported control character at index " + i
                    + " and cannot be written into a generated Dockerfile");
            }
        }
        return value;
    }

    protected static String validateImageReference(String source, String value) throws MojoExecutionException {
        String sanitized = validateDockerfileValue(source, value);
        try {
            ImageReference.parse(sanitized);
        } catch (InvalidImageReferenceException e) {
            throw new MojoExecutionException(source + " is not a valid Docker image reference: " + sanitized, e);
        }
        return sanitized;
    }

    protected static String validateExposedPorts(String source, String value) throws MojoExecutionException {
        String sanitized = validateDockerfileValue(source, value);
        for (String token : sanitized.split("\\s+")) {
            if (token.isEmpty()) {
                continue;
            }
            if (!token.matches("\\d+(/(?:tcp|udp))?")) {
                throw new MojoExecutionException(source + " contains an invalid exposed port token: " + token);
            }
        }
        return sanitized;
    }

    protected static String validateDownloadUrl(String source, String value) throws MojoExecutionException {
        String sanitized = validateDockerfileValue(source, value);
        try {
            URI uri = new URI(sanitized);
            String scheme = uri.getScheme();
            if (!"https".equalsIgnoreCase(scheme) && !"http".equalsIgnoreCase(scheme)) {
                throw new MojoExecutionException(source + " must use an http or https URL: " + sanitized);
            }
            if (!uri.isAbsolute()) {
                throw new MojoExecutionException(source + " must be an absolute URL: " + sanitized);
            }
        } catch (URISyntaxException e) {
            throw new MojoExecutionException(source + " is not a valid URL: " + sanitized, e);
        }
        return sanitized;
    }

    protected static String jsonStringLiteral(String source, String value) throws MojoExecutionException {
        return "\"" + escapeJsonString(source, value) + "\"";
    }

    protected static String escapeJsonString(String source, String value) throws MojoExecutionException {
        String sanitized = validateDockerfileValue(source, value);
        var result = new StringBuilder(sanitized.length() + 8);
        for (int i = 0; i < sanitized.length(); i++) {
            char c = sanitized.charAt(i);
            switch (c) {
                case '"' -> result.append("\\\"");
                case '\\' -> result.append("\\\\");
                case '\b' -> result.append("\\b");
                case '\f' -> result.append("\\f");
                case '\n' -> result.append("\\n");
                case '\r' -> result.append("\\r");
                case '\t' -> result.append("\\t");
                default -> {
                    if (c < 0x20) {
                        result.append(String.format("\\u%04x", (int) c));
                    } else {
                        result.append(c);
                    }
                }
            }
        }
        return result.toString();
    }

    protected static String shellLiteral(String source, String value) throws MojoExecutionException {
        return quoteShellLiteral(source, validateDockerfileValue(source, value));
    }

    /**
     * @return Networking mode for the RUN instructions during build (if any).
     */
    protected Optional<String> getNetworkMode() {
        return Optional.ofNullable(networkMode);
    }

    /**
     * @return Map of proxy-related build arguments for Docker builds.
     */
    protected Map<String, String> getProxyBuildArgs() {
        var proxyArgs = new java.util.HashMap<String, String>();
        
        // HTTP proxy configuration from standard JVM properties
        String httpProxyHost = System.getProperty("http.proxyHost");
        String httpProxyPort = System.getProperty("http.proxyPort", "80");
        if (StringUtils.hasText(httpProxyHost)) {
            String httpProxy = "http://" + httpProxyHost + ":" + httpProxyPort;
            proxyArgs.put("HTTP_PROXY", httpProxy);
            proxyArgs.put("http_proxy", httpProxy);
        }
        
        // HTTPS proxy configuration from standard JVM properties
        String httpsProxyHost = System.getProperty("https.proxyHost");
        String httpsProxyPort = System.getProperty("https.proxyPort", "443");
        if (StringUtils.hasText(httpsProxyHost)) {
            String httpsProxy = "http://" + httpsProxyHost + ":" + httpsProxyPort;
            proxyArgs.put("HTTPS_PROXY", httpsProxy);
            proxyArgs.put("https_proxy", httpsProxy);
        }
        
        // No proxy configuration from standard JVM properties
        String nonProxyHosts = System.getProperty("http.nonProxyHosts");
        if (StringUtils.hasText(nonProxyHosts)) {
            // Convert Java format (e.g., "*.company.com|localhost") to standard format (e.g., "*.company.com,localhost")
            String noProxy = nonProxyHosts.replace("|", ",");
            proxyArgs.put("NO_PROXY", noProxy);
            proxyArgs.put("no_proxy", noProxy);
        }
        
        return proxyArgs;
    }

    /**
     * @return the base image to use for the Dockerfile.
     */
    protected String getBaseImage() {
        return JibMicronautExtension.determineBaseImage(JibMicronautExtension.getJdkVersion(mavenSession), MicronautRuntime.valueOf(micronautRuntime.toUpperCase()).getBuildStrategy());
    }

    /**
     * Adds cmd to docker oracle cloud function file.
     *
     * @param dockerfile the docker file
     */
    protected void oracleCloudFunctionCmd(File dockerfile) throws IOException, MojoExecutionException {
        if (appArguments != null && !appArguments.isEmpty()) {
            getLog().info("Using application arguments: " + appArguments);
            com.google.common.io.Files.asCharSink(dockerfile, Charset.defaultCharset(), FileWriteMode.APPEND)
                .write(System.lineSeparator() + getCmd());
        } else {
            com.google.common.io.Files.asCharSink(dockerfile, Charset.defaultCharset(), FileWriteMode.APPEND).write(System.lineSeparator() + ORACLE_CLOUD_FUNCTION_DEFAULT_CMD);
        }
    }

}
