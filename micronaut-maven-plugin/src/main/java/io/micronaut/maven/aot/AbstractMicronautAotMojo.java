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

import io.micronaut.maven.AbstractMicronautMojo;
import io.micronaut.maven.Packaging;
import io.micronaut.maven.services.CompilerService;
import org.apache.commons.io.FileUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.resolution.DependencyResolutionException;

import java.io.File;
import java.io.IOException;

/**
 * Abstract Mojo for Micronaut AOT.
 */
public abstract class AbstractMicronautAotMojo extends AbstractMicronautMojo {

    protected final CompilerService compilerService;

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

    public AbstractMicronautAotMojo(CompilerService compilerService, MavenProject mavenProject) {
        this.compilerService = compilerService;
        this.mavenProject = mavenProject;
    }

    abstract void onSuccess(File outputDir) throws MojoExecutionException;

    protected final File getBaseOutputDirectory() {
        File targetDirectory = new File(mavenProject.getBuild().getDirectory(), "aot");
        return new File(targetDirectory, runtime);
    }

    protected final File outputFile(String name) {
        return new File(getBaseOutputDirectory(), name);
    }

    @Override
    public final void execute() throws MojoExecutionException {
        if (!enabled) {
            return;
        }
        validateRuntime();
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

    private void clean(File baseOutputDirectory) throws IOException {
        if (baseOutputDirectory.exists()) {
            getLog().debug("Deleting " + baseOutputDirectory.getAbsolutePath());
            FileUtils.deleteDirectory(baseOutputDirectory);
        }
        baseOutputDirectory.mkdirs();
    }

    private void validateRuntime() {
        Packaging packaging = Packaging.of(mavenProject.getPackaging());
        AotRuntime aotRuntime = AotRuntime.valueOf(runtime.toUpperCase());
        switch (packaging) {
            case JAR, DOCKER_CRAC, DOCKER -> {
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

    private void warnRuntimeMismatchAndSetCorrectValue(final AotRuntime correctRuntime) {
        String correctRuntimeString = correctRuntime.name().toLowerCase();
        getLog().warn("Packaging is set to [" + mavenProject.getPackaging() + "], but Micronaut AOT runtime is set to [" + runtime + "]. Setting AOT runtime to: [" + correctRuntimeString + "]");
        this.runtime = correctRuntimeString;
    }

    protected abstract void doExecute() throws DependencyResolutionException, MojoExecutionException;

    abstract String getName();
}
