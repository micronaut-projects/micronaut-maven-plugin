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
package io.micronaut.maven.info;

import io.micronaut.maven.AbstractMicronautMojo;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.util.Map;

/**
 * Generates Micronaut management info endpoint build and git metadata.
 *
 * @author Micronaut Authors
 * @since 5.0.1
 */
@Mojo(name = GenerateInfoMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_RESOURCES, threadSafe = true)
public class GenerateInfoMojo extends AbstractMicronautMojo {

    /**
     * Name of the generate info mojo.
     */
    public static final String MOJO_NAME = "generate-info";

    private static final String MICRONAUT_INFO_PREFIX = "micronaut.info";

    /**
     * Reference to the Maven project on which the plugin is invoked.
     */
    @Parameter(defaultValue = "${project}", readonly = true, required = true)
    private MavenProject project;

    /**
     * Reference to the Maven session.
     */
    @Parameter(defaultValue = "${session}", readonly = true)
    private MavenSession session;

    /**
     * Whether to skip metadata generation.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".skip", defaultValue = "false")
    private boolean skip;

    /**
     * Whether to generate build metadata.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".build.enabled", defaultValue = "true")
    private boolean buildEnabled;

    /**
     * Whether to generate git metadata.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".git.enabled", defaultValue = "true")
    private boolean gitEnabled;

    /**
     * Whether to fail the build when git metadata is enabled but unavailable.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".failOnNoGit", defaultValue = "false")
    private boolean failOnNoGit;

    /**
     * Whether to include a boolean dirty-state flag in generated git metadata.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".includeDirty", defaultValue = "true")
    private boolean includeDirty;

    /**
     * Whether to include the configured origin remote URL in generated git metadata.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".includeRemoteUrl", defaultValue = "false")
    private boolean includeRemoteUrl;

    /**
     * Whether to include local git user identity in generated git metadata.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".includeUser", defaultValue = "false")
    private boolean includeUser;

    /**
     * Build timestamp to use for build metadata. When unset, {@code project.build.outputTimestamp}
     * is used if present, then the Maven session start time, then the current instant.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".time")
    private String time;

    /**
     * Build metadata output file.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".build.outputFile", defaultValue = "${project.build.outputDirectory}/META-INF/build-info.properties", required = true)
    private File buildOutputFile;

    /**
     * Git metadata output file.
     *
     * @since 5.0.1
     */
    @Parameter(property = MICRONAUT_INFO_PREFIX + ".git.outputFile", defaultValue = "${project.build.outputDirectory}/git.properties", required = true)
    private File gitOutputFile;

    /**
     * Additional build metadata properties. Keys are written as provided.
     *
     * @since 5.0.1
     */
    @Parameter
    private Map<String, String> additionalBuildProperties;

    /**
     * Additional git metadata properties. Keys are written as provided.
     *
     * @since 5.0.1
     */
    @Parameter
    private Map<String, String> additionalGitProperties;

    @Override
    public void execute() throws MojoExecutionException {
        if (skip) {
            getLog().info("Skipping Micronaut info metadata generation");
            return;
        }

        Instant buildTime;
        try {
            buildTime = BuildInfoGenerator.resolveBuildTime(time, project, session);
        } catch (IllegalArgumentException e) {
            throw new MojoExecutionException("Error resolving Micronaut build info timestamp", e);
        }
        if (buildEnabled) {
            try {
                PropertiesFileWriter.write(buildOutputFile.toPath(), BuildInfoGenerator.generate(project, buildTime, additionalBuildProperties));
                getLog().info("Generated Micronaut build info at " + buildOutputFile.getAbsolutePath());
            } catch (IOException e) {
                throw new MojoExecutionException("Error generating Micronaut build info", e);
            }
        } else {
            getLog().debug("Micronaut build info generation is disabled");
        }

        if (gitEnabled) {
            GitInfoGenerator gitInfoGenerator = new GitInfoGenerator(project.getBasedir().toPath(), includeDirty, includeRemoteUrl, includeUser);
            GitInfoGenerator.Result result = gitInfoGenerator.generate(additionalGitProperties);
            if (result.properties().isEmpty()) {
                String message = "Git metadata is unavailable: " + result.message();
                if (failOnNoGit) {
                    throw new MojoExecutionException(message);
                }
                getLog().warn(message + "; skipping git.properties generation");
                return;
            }
            try {
                PropertiesFileWriter.write(gitOutputFile.toPath(), result.properties());
                getLog().info("Generated Micronaut git info at " + gitOutputFile.getAbsolutePath());
            } catch (IOException e) {
                throw new MojoExecutionException("Error generating Micronaut git info", e);
            }
        } else {
            getLog().debug("Micronaut git info generation is disabled");
        }
    }
}
