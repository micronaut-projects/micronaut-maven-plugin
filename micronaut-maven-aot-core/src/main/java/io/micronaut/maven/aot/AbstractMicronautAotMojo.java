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

import io.micronaut.maven.aot.internal.AotCompilerService;
import io.micronaut.maven.aot.internal.AotPackaging;
import io.micronaut.maven.aot.internal.JansiLog;
import org.apache.commons.io.FileUtils;
import org.apache.maven.model.Exclusion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.resolution.DependencyResolutionException;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Abstract Mojo for Micronaut AOT.
 */
public abstract class AbstractMicronautAotMojo extends AbstractMojo {

    protected final AotCompilerService compilerService;
    protected final MavenProject mavenProject;

    /**
     * Micronaut AOT runtime. Possible values: <code>jit</code>, <code>native</code>.
     */
    @Parameter(property = "micronaut.aot.runtime", required = true, defaultValue = "jit")
    protected String runtime;

    /**
     * Micronaut AOT version.
     */
    @Parameter(property = "micronaut.aot.version", required = true)
    protected String micronautAotVersion;

    /**
     * Whether to enable or disable Micronaut AOT.
     */
    @Parameter(property = "micronaut.aot.enabled", defaultValue = "false")
    protected boolean enabled;

    /**
     * Directory where compiled application classes are.
     */
    @Parameter(defaultValue = "${project.build.outputDirectory}", required = true)
    protected File outputDirectory;

    /**
     * Packages that would be excluded from the AOT processing.
     *
     * @since 4.11.0
     */
    @Parameter(property = "exclusions")
    protected List<Exclusion> aotExclusions;

    protected AbstractMicronautAotMojo(AotCompilerService compilerService, MavenProject mavenProject) {
        this.compilerService = compilerService;
        this.mavenProject = mavenProject;
    }

    @Override
    public void setLog(Log log) {
        super.setLog(new JansiLog(log));
    }

    protected final File getBaseOutputDirectory() {
        File targetDirectory = new File(mavenProject.getBuild().getDirectory(), "aot");
        return new File(targetDirectory, runtime);
    }

    protected final File outputFile(String name) {
        return new File(getBaseOutputDirectory(), name);
    }

    @Override
    public final void execute() throws MojoExecutionException {
        if (!enabled || !shouldExecute()) {
            return;
        }
        if (alignRuntimeWithPackaging()) {
            validateRuntime();
        }
        getLog().info("Running Micronaut AOT " + micronautAotVersion + " " + getName());
        try {
            File baseOutputDirectory = getBaseOutputDirectory();
            clean(baseOutputDirectory);
            clean(outputDirectory);
            doExecute();
            onSuccess(baseOutputDirectory);
        } catch (DependencyResolutionException | IOException e) {
            throw new MojoExecutionException("Unable to generate AOT optimizations", e);
        }
    }

    /**
     * Allows concrete plugins to suppress execution for specific project layouts.
     *
     * @return {@code true} when AOT execution should proceed
     */
    protected boolean shouldExecute() {
        return true;
    }

    /**
     * Controls whether the configured runtime should be normalized against the current project packaging.
     *
     * @return {@code true} to enforce the integrated packaging/runtime mapping
     */
    protected boolean alignRuntimeWithPackaging() {
        return true;
    }

    abstract void onSuccess(File outputDir) throws MojoExecutionException;

    protected abstract void doExecute() throws DependencyResolutionException, MojoExecutionException;

    abstract String getName();

    private void clean(File directory) throws IOException {
        if (directory.exists()) {
            getLog().debug("Deleting " + directory.getAbsolutePath());
            FileUtils.deleteDirectory(directory);
        }
        directory.mkdirs();
    }

    private void validateRuntime() {
        AotPackaging packaging = AotPackaging.of(mavenProject.getPackaging());
        AotRuntime aotRuntime = AotRuntime.valueOf(runtime.toUpperCase());
        switch (packaging) {
            case JAR, DOCKER, DOCKER_CRAC -> {
                if (aotRuntime != AotRuntime.JIT) {
                    warnRuntimeMismatchAndSetCorrectValue(AotRuntime.JIT);
                }
            }
            case NATIVE_IMAGE, DOCKER_NATIVE -> {
                if (aotRuntime != AotRuntime.NATIVE) {
                    warnRuntimeMismatchAndSetCorrectValue(AotRuntime.NATIVE);
                }
            }
            default -> throw new IllegalArgumentException("Unsupported packaging: " + packaging);
        }
    }

    private void warnRuntimeMismatchAndSetCorrectValue(AotRuntime correctRuntime) {
        String correctRuntimeString = correctRuntime.name().toLowerCase();
        getLog().warn("Packaging is set to [" + mavenProject.getPackaging() + "], but Micronaut AOT runtime is set to [" + runtime + "]. Setting AOT runtime to: [" + correctRuntimeString + "]");
        runtime = correctRuntimeString;
    }
}
