/*
 * Copyright 2017-2021 original authors
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
package io.micronaut.maven.openapi;

import io.micronaut.maven.AbstractMicronautMojo;
import io.micronaut.openapi.generator.MicronautCodeGeneratorEntryPoint;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.List;

/**
 * Base class for OpenAPI generator mojos. This provides the common
 * parameters for all generators and the invoker logic. Subclasses
 * must implement the {@link #isEnabled()} and {@link #configureBuilder(MicronautCodeGeneratorEntryPoint.Builder)}
 * methods.
 */
public abstract class AbstractOpenApiMojo extends AbstractMicronautMojo {
    @Parameter(property = "micronaut.openapi.definition", defaultValue = "io.micronaut.openapi.invoker", required = true)
    protected File definitionFile;

    @Parameter(property = "micronaut.openapi.invoker.package.name", defaultValue = "io.micronaut.openapi.invoker", required = true)
    protected String invokerPackageName;

    @Parameter(property = "micronaut.openapi.api.package.name", defaultValue = "io.micronaut.openapi.api", required = true)
    protected String apiPackageName;

    @Parameter(property = "micronaut.openapi.model.package.name", defaultValue = "io.micronaut.openapi.model", required = true)
    protected String modelPackageName;

    @Parameter(property = "micronaut.openapi.use.bean.validation", defaultValue = "true", required = true)
    protected boolean useBeanValidation;

    @Parameter(property = "micronaut.openapi.use.optional", defaultValue = "false", required = true)
    protected boolean useOptional;

    @Parameter(property = "micronaut.openapi.use.reactive", defaultValue = "true", required = true)
    protected boolean useReactive;

    @Parameter(property = "micronaut.openapi.outputs", required = true, defaultValue = "apis,models")
    protected List<String> outputKinds;

    @Parameter(defaultValue = "${project.build.directory}/generated-sources/openapi", required = true)
    protected File outputDirectory;

    @Parameter(defaultValue = "${project}", readonly = true)
    private MavenProject project;

    /**
     * Determines if this mojo must be executed.
     * @return true if the mojo is enabled
     */
    protected abstract boolean isEnabled();

    /**
     * Configures the OpenAPI generator. When this method is called,
     * common properties shared by all generators have already been
     * configured, so this method should only take care of configuring
     * the generator specific parameters.
     * @param builder the generator configuration builder
     */
    protected abstract void configureBuilder(MicronautCodeGeneratorEntryPoint.Builder builder);

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (!isEnabled()) {
            getLog().debug(this.getClass().getSimpleName() + " is disabled");
            return;
        }
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        var builder = MicronautCodeGeneratorEntryPoint.builder()
                .withDefinitionFile(definitionFile.toURI())
                .withOutputDirectory(outputDirectory)
                .withOutputs(
                        outputKinds.stream().map(String::toUpperCase).map(MicronautCodeGeneratorEntryPoint.OutputKind::valueOf).toList().toArray(new MicronautCodeGeneratorEntryPoint.OutputKind[0])
                )
                .withOptions(options -> {
                    options.withInvokerPackage(invokerPackageName);
                    options.withApiPackage(apiPackageName);
                    options.withModelPackage(modelPackageName);
                    options.withBeanValidation(useBeanValidation);
                    options.withOptional(useOptional);
                    options.withReactive(useReactive);
                });
        configureBuilder(builder);
        builder.build().generate();
    }
}
