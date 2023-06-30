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
import io.micronaut.openapi.generator.MicronautCodeGeneratorBuilder;
import io.micronaut.openapi.generator.MicronautCodeGeneratorEntryPoint;
import io.micronaut.openapi.generator.SerializationLibraryKind;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.List;

/**
 * Base class for OpenAPI generator mojos. This provides the common
 * parameters for all generators and the invoker logic. Subclasses
 * must implement the {@link #isEnabled()} and {@link #configureBuilder(MicronautCodeGeneratorBuilder)}
 * methods.
 */
public abstract class AbstractOpenApiMojo extends AbstractMicronautMojo {
    static final String MICRONAUT_OPENAPI_PREFIX = "micronaut.openapi";
    static final String IO_MICRONAUT_OPENAPI_PREFIX = "io.micronaut.openapi";

    /**
     * The OpenAPI specification file path relative to the project's root path.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".definition", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".invoker", required = true)
    protected File definitionFile;

    /**
     * The name of the package that can be used for various classes required for invocation.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".invoker.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".invoker", required = true)
    protected String invokerPackageName;

    /**
     * The package name for the APIs (controller interfaces).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".api.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".api", required = true)
    protected String apiPackageName;

    /**
     * The package name for the model classes.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".model.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".model", required = true)
    protected String modelPackageName;

    /**
     * Whether to generate validation annotations for models and APIs.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.bean.validation", defaultValue = "true", required = true)
    protected boolean useBeanValidation;

    /**
     * Whether to use {@link java.util.Optional} for non-required model properties and API parameters.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.optional", defaultValue = "false", required = true)
    protected boolean useOptional;

    /**
     * Whether to use reactor types for operation responses.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.reactive", defaultValue = "true", required = true)
    protected boolean useReactive;

    /**
     * Comma-separated values of output kinds to generate. The values are defined by the
     * {@link MicronautCodeGeneratorEntryPoint.OutputKind} enum.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".outputs", required = true, defaultValue = "apis,models,supporting_files")
    protected List<String> outputKinds;

    /**
     * The output directory to which all the sources will be generated.
     */
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
    protected abstract void configureBuilder(MicronautCodeGeneratorBuilder builder) throws MojoExecutionException;

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
                    options.withSerializationLibrary(SerializationLibraryKind.MICRONAUT_SERDE_JACKSON);
                });
        configureBuilder(builder);
        builder.build().generate();
    }
}
