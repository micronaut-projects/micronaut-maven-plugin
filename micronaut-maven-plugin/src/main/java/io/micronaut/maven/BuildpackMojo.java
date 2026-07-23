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

import io.micronaut.maven.jib.JibConfigurationService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.StringUtils;

import javax.inject.Inject;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.function.Consumer;

/**
 * Builds a JVM application image with Cloud Native Buildpacks through the pack CLI.
 *
 * @author Sergio del Amo
 * @since 5.0.0
 */
@Mojo(name = BuildpackMojo.BUILDPACK_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class BuildpackMojo extends AbstractMicronautMojo {

    public static final String BUILDPACK_PACKAGING = "buildpack";
    static final String DEFAULT_PACK_EXECUTABLE = "pack";
    static final String ENVIRONMENT_PROPERTY_PREFIX = "micronaut.buildpack.environment.";

    /**
     * The pack CLI executable to run.
     */
    @Parameter(property = "micronaut.buildpack.executable", defaultValue = DEFAULT_PACK_EXECUTABLE)
    String packExecutable;

    /**
     * The Cloud Native Buildpacks builder image to use.
     */
    @Parameter(property = "micronaut.buildpack.builder-image")
    String builderImage;

    /**
     * Optional run image passed to pack.
     */
    @Parameter(property = "micronaut.buildpack.run-image")
    String runImage;

    /**
     * Output image name. If omitted, jib.to.image is used when configured.
     */
    @Parameter(property = "micronaut.buildpack.image-name")
    String imageName;

    /**
     * Optional artifact path to pass to pack. Defaults to the packaged project artifact.
     */
    @Parameter(property = "micronaut.buildpack.artifact")
    File artifact;

    /**
     * Builder environment variables. Command-line properties may also use micronaut.buildpack.environment.KEY=value.
     */
    @Parameter
    Map<String, String> environment;

    /**
     * Whether to pass --trust-builder to pack. Keep the default conservative.
     */
    @Parameter(property = "micronaut.buildpack.trust-builder", defaultValue = "false")
    boolean trustBuilder;

    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final JibConfigurationService jibConfigurationService;
    private final BuildpackCommandRunner commandRunner;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public BuildpackMojo(MavenProject mavenProject, MavenSession mavenSession,
                         JibConfigurationService jibConfigurationService) {
        this(mavenProject, mavenSession, jibConfigurationService, new DefaultBuildpackCommandRunner());
    }

    BuildpackMojo(MavenProject mavenProject, MavenSession mavenSession,
                  JibConfigurationService jibConfigurationService, BuildpackCommandRunner commandRunner) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.jibConfigurationService = jibConfigurationService;
        this.commandRunner = commandRunner;
    }

    @Override
    public void execute() throws MojoExecutionException {
        BuildpackInvocation invocation = buildInvocation();
        verifyPackExecutable(invocation.packExecutable());
        List<String> command = invocation.command();
        getLog().info("Running " + String.join(" ", redactedCommand(command)));
        int exitCode = runPack(command);
        if (exitCode != 0) {
            throw new MojoExecutionException("pack build failed with exit code " + exitCode
                + ". Ensure the Docker daemon is running, the builder image is trusted and reachable, and the "
                + "micronaut.buildpack.* configuration is valid.");
        }
    }

    final BuildpackInvocation buildInvocation() throws MojoExecutionException {
        String executable = requireText("micronaut.buildpack.executable", packExecutable);
        String builder = AbstractDockerMojo.validateImageReference(
            "micronaut.buildpack.builder-image",
            requireText("micronaut.buildpack.builder-image", builderImage)
        );
        String image = AbstractDockerMojo.validateImageReference(
            "micronaut.buildpack.image-name",
            resolveImageName()
        );
        File applicationArtifact = resolveArtifact();
        if (!applicationArtifact.isFile()) {
            throw new MojoExecutionException("Buildpack application artifact does not exist: "
                + applicationArtifact.getAbsolutePath()
                + ". Run the package phase or set micronaut.buildpack.artifact to an existing runnable jar.");
        }

        var command = new ArrayList<String>();
        command.add(executable);
        command.add("build");
        command.add(image);
        command.add("--builder");
        command.add(builder);
        command.add("--path");
        command.add(applicationArtifact.getAbsolutePath());
        if (StringUtils.isNotEmpty(runImage)) {
            command.add("--run-image");
            command.add(AbstractDockerMojo.validateImageReference("micronaut.buildpack.run-image", runImage));
        }
        if (trustBuilder) {
            command.add("--trust-builder");
        }
        for (Map.Entry<String, String> entry : resolveEnvironment().entrySet()) {
            command.add("--env");
            command.add(validateEnvironmentVariable(entry.getKey(), entry.getValue()));
        }
        return new BuildpackInvocation(executable, command);
    }

    private void verifyPackExecutable(String executable) throws MojoExecutionException {
        try {
            commandRunner.verifyPack(executable, mavenProject.getBasedir(), getLog()::debug);
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to execute the pack CLI '" + executable
                + "'. Install Cloud Native Buildpacks pack or set micronaut.buildpack.executable to its path.", e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new MojoExecutionException("Interrupted while checking the pack CLI", e);
        }
    }

    private int runPack(List<String> command) throws MojoExecutionException {
        try {
            return commandRunner.run(command, mavenProject.getBasedir(), getLog()::info);
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to run pack build. Ensure pack is installed and executable.", e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new MojoExecutionException("Interrupted while running pack build", e);
        }
    }

    private String resolveImageName() throws MojoExecutionException {
        if (StringUtils.isNotEmpty(imageName)) {
            return imageName;
        }
        if (jibConfigurationService != null) {
            return jibConfigurationService.getToImage()
                .filter(StringUtils::isNotEmpty)
                .orElseThrow(this::missingImageName);
        }
        throw missingImageName();
    }

    private MojoExecutionException missingImageName() {
        return new MojoExecutionException("Buildpack image name is required. Set micronaut.buildpack.image-name"
            + " or configure jib.to.image as a compatibility fallback.");
    }

    private File resolveArtifact() throws MojoExecutionException {
        if (artifact != null) {
            return absoluteFile(artifact);
        }
        if (mavenProject.getArtifact() != null && mavenProject.getArtifact().getFile() != null) {
            return absoluteFile(mavenProject.getArtifact().getFile());
        }
        String finalName = mavenProject.getBuild() == null ? null : mavenProject.getBuild().getFinalName();
        String directory = mavenProject.getBuild() == null ? null : mavenProject.getBuild().getDirectory();
        if (StringUtils.isEmpty(finalName) || StringUtils.isEmpty(directory)) {
            throw new MojoExecutionException("Unable to determine the buildpack application artifact. Set micronaut.buildpack.artifact.");
        }
        return absoluteFile(new File(directory, finalName + ".jar"));
    }

    private File absoluteFile(File file) {
        return file.isAbsolute() ? file : new File(mavenProject.getBasedir(), file.getPath());
    }

    private Map<String, String> resolveEnvironment() throws MojoExecutionException {
        var resolved = new TreeMap<String, String>();
        addEnvironmentProperties(resolved, mavenProject == null ? null : mavenProject.getProperties());
        addEnvironmentProperties(resolved, mavenSession == null ? null : mavenSession.getSystemProperties());
        addEnvironmentProperties(resolved, mavenSession == null ? null : mavenSession.getUserProperties());
        if (environment != null) {
            resolved.putAll(environment);
        }
        for (Map.Entry<String, String> entry : resolved.entrySet()) {
            validateEnvironmentVariable(entry.getKey(), entry.getValue());
        }
        return Collections.unmodifiableMap(resolved);
    }

    private static void addEnvironmentProperties(Map<String, String> resolved, Properties properties) {
        if (properties == null) {
            return;
        }
        properties.stringPropertyNames().stream()
            .filter(name -> name.startsWith(ENVIRONMENT_PROPERTY_PREFIX))
            .forEach(name -> resolved.put(name.substring(ENVIRONMENT_PROPERTY_PREFIX.length()), properties.getProperty(name)));
    }

    private static String validateEnvironmentVariable(String name, String value) throws MojoExecutionException {
        if (StringUtils.isEmpty(name) || !name.matches("[A-Za-z_]\\w*")) {
            throw new MojoExecutionException("Buildpack environment variable names must match [A-Za-z_]\\w*: " + name);
        }
        return name + "=" + AbstractDockerMojo.validateDockerfileValue(ENVIRONMENT_PROPERTY_PREFIX + name, value == null ? "" : value);
    }

    private static String requireText(String name, String value) throws MojoExecutionException {
        if (StringUtils.isEmpty(value)) {
            throw new MojoExecutionException(name + " is required for buildpack packaging");
        }
        return value;
    }

    private static List<String> redactedCommand(List<String> command) {
        var redacted = new ArrayList<String>(command.size());
        boolean redactNext = false;
        for (String argument : command) {
            if (redactNext) {
                int equals = argument.indexOf('=');
                redacted.add(equals > 0 ? argument.substring(0, equals + 1) + "<redacted>" : "<redacted>");
                redactNext = false;
            } else {
                redacted.add(argument);
                redactNext = "--env".equals(argument);
            }
        }
        return redacted;
    }

    record BuildpackInvocation(String packExecutable, List<String> command) {
    }

    interface BuildpackCommandRunner {
        void verifyPack(String executable, File workingDirectory, Consumer<String> output) throws IOException, InterruptedException;

        int run(List<String> command, File workingDirectory, Consumer<String> output) throws IOException, InterruptedException;
    }

    private static final class DefaultBuildpackCommandRunner implements BuildpackCommandRunner {

        @Override
        public void verifyPack(String executable, File workingDirectory, Consumer<String> output) throws IOException, InterruptedException {
            int exitCode = run(List.of(executable, "--version"), workingDirectory, output);
            if (exitCode != 0) {
                throw new IOException("pack --version exited with code " + exitCode);
            }
        }

        @Override
        public int run(List<String> command, File workingDirectory, Consumer<String> output) throws IOException, InterruptedException {
            ProcessBuilder processBuilder = new ProcessBuilder(command).redirectErrorStream(true);
            if (workingDirectory != null) {
                processBuilder.directory(workingDirectory);
            }
            Process process = processBuilder.start();
            try (var reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    output.accept(line);
                }
            }
            return process.waitFor();
        }
    }
}
