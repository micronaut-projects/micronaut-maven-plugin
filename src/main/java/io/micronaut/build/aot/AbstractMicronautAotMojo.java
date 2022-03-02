/*
 * Copyright 2003-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.build.aot;

import io.micronaut.build.services.CompilerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.resolution.DependencyResolutionException;

import java.io.File;

public abstract class AbstractMicronautAotMojo extends AbstractMojo {

    protected final CompilerService compilerService;

    protected final MavenProject mavenProject;

    protected final MavenSession mavenSession;

    protected final RepositorySystem repositorySystem;

    /**
     * Micronaut AOT runtime. Possible values: <code>jit</code>, <code>native</code>.
     */
    @Parameter(property = "micronaut.aot.runtime", required = true, defaultValue = "jit")
    protected String runtime;

    /**
     * Micronaut AOT version.
     */
    @Parameter(property = "micronaut.aot.version", required = true, defaultValue = "1.0.0-M7")
    protected String micronautAotVersion;

    /**
     * Whether to enable or disable Micronaut AOT.
     */
    @Parameter(property = "micronaut.aot.enabled", required = false, defaultValue = "false")
    protected boolean enabled;


    public AbstractMicronautAotMojo(CompilerService compilerService, MavenProject mavenProject, MavenSession mavenSession, RepositorySystem repositorySystem) {
        this.compilerService = compilerService;
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.repositorySystem = repositorySystem;
    }

    protected void onSuccess(File outputDir) {

    }

    protected final File getBaseOutputDirectory() {
        File targetDirectory = new File(mavenProject.getBuild().getDirectory(), "aot");
        return new File(targetDirectory, runtime);
    }

    protected final File outputFile(String name) {
        return new File(getBaseOutputDirectory(), name);
    }

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (!enabled) {
            return;
        }
        getLog().info("Running Micronaut AOT " + micronautAotVersion);
        getLog().debug("Invoking " + getClass().getSimpleName());
        try {
            getBaseOutputDirectory().mkdirs();
            doExecute();
            onSuccess(getBaseOutputDirectory());
        } catch (DependencyResolutionException e) {
            throw new MojoExecutionException("Unable to generate AOT optimizations", e);
        }
    }

    protected abstract void doExecute() throws DependencyResolutionException, MojoExecutionException;
}
