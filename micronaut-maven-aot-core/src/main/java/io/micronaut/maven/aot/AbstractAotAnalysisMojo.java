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
package io.micronaut.maven.aot;

import io.micronaut.aot.std.sourcegen.KnownMissingTypesSourceGenerator;
import io.micronaut.maven.aot.internal.AotCompilerService;
import io.micronaut.maven.aot.internal.AotDependencyResolutionService;
import io.micronaut.maven.aot.internal.AotExecutorService;
import org.apache.commons.io.FileUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * Shared implementation for the AOT analysis goal.
 */
public abstract class AbstractAotAnalysisMojo extends AbstractMicronautAotCliMojo {

    public static final String NAME = "aot-analysis";
    public static final String AOT_PROPERTIES_FILE_NAME = "aot.properties";

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}", required = true)
    private File baseDirectory;

    /**
     * Micronaut AOT configuration file. Run the <a href="aot-sample-config-mojo.html"><code>aot-sample-config</code> goal</a> to
     * see all the possible options.
     */
    @Parameter(property = "micronaut.aot.config", defaultValue = AOT_PROPERTIES_FILE_NAME)
    private File configFile;

    @Inject
    protected AbstractAotAnalysisMojo(AotCompilerService compilerService,
                                      AotExecutorService executorService,
                                      MavenProject mavenProject,
                                      AotDependencyResolutionService dependencyResolutionService,
                                      MavenSession mavenSession,
                                      ToolchainManager toolchainManager) {
        super(compilerService, executorService, mavenProject, dependencyResolutionService, mavenSession, toolchainManager);
    }

    @Override
    protected List<String> getExtraArgs() throws MojoExecutionException {
        ArrayList<String> args = new ArrayList<>();
        args.add("--output");
        File generated = outputFile("generated");
        args.add(generated.getAbsolutePath());
        File effectiveConfigFile = writeEffectiveConfigFile();
        args.add("--config");
        args.add(effectiveConfigFile.getAbsolutePath());
        return args;
    }

    @Override
    protected void onSuccess(File outputDir) throws MojoExecutionException {
        Path generated = outputDir.toPath().resolve("generated");
        Path generatedClasses = generated.resolve("classes");
        Path targetOutputDirectory = outputDirectory.toPath().toAbsolutePath().normalize();
        try {
            FileUtils.copyDirectory(generatedClasses.toFile(), outputDirectory);
            try (Stream<String> linesStream = Files.lines(generated.resolve("logs").resolve("resource-filter.txt"))) {
                linesStream.forEach(toRemove -> {
                    String sanitized = toRemove.strip();
                    if (sanitized.isEmpty() || ".".equals(sanitized)) {
                        return;
                    }
                    final Path candidate;
                    try {
                        candidate = targetOutputDirectory.resolve(Path.of(sanitized)).normalize();
                    } catch (InvalidPathException e) {
                        getLog().warn("Skipping invalid deletion entry: " + toRemove, e);
                        return;
                    }
                    if (!candidate.startsWith(targetOutputDirectory)) {
                        getLog().warn("Skipping deletion outside output directory: " + toRemove);
                        return;
                    }
                    try {
                        Files.delete(candidate);
                        getLog().debug("Removed " + toRemove);
                    } catch (IOException e) {
                        if (!(e instanceof NoSuchFileException)) {
                            getLog().warn("Error while deleting " + toRemove, e);
                        }
                    }
                });
            }
        } catch (IOException e) {
            throw new MojoExecutionException("Error when copying the Micronaut AOT generated classes into the target directory", e);
        }
    }

    @Override
    String getName() {
        return NAME;
    }

    private File writeEffectiveConfigFile() throws MojoExecutionException {
        File userProvidedFile = configFile == null ? new File(baseDirectory, AOT_PROPERTIES_FILE_NAME) : configFile;
        Properties props = new Properties();
        if (userProvidedFile.exists()) {
            try (InputStream in = Files.newInputStream(userProvidedFile.toPath())) {
                getLog().info("Using AOT configuration file: " + userProvidedFile.getAbsolutePath());
                props.load(in);
            } catch (IOException e) {
                throw new MojoExecutionException("Unable to parse configuration file", e);
            }
        }
        if (!props.containsKey(KnownMissingTypesSourceGenerator.OPTION.key())) {
            props.put(KnownMissingTypesSourceGenerator.OPTION.key(), String.join(",", Constants.TYPES_TO_CHECK));
        }
        File effectiveConfig = outputFile("effective-" + AOT_PROPERTIES_FILE_NAME);
        try (OutputStream out = Files.newOutputStream(effectiveConfig.toPath())) {
            props.store(out, "Effective AOT configuration");
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to write effective AOT configuration file", e);
        }
        return effectiveConfig;
    }
}
