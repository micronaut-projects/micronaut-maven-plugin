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

import io.micronaut.openapi.generator.MicronautCodeGeneratorBuilder;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Generates an OpenAPI server.
 * The sources are generated in the target directory.
 */
@Mojo(name = OpenApiServerMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class OpenApiServerMojo extends AbstractOpenApiMojo {

    public static final String MOJO_NAME = "generate-openapi-server";

    private static final String SERVER_PREFIX = MICRONAUT_OPENAPI_PREFIX + ".server.";

    /**
     * The package name of the controller if controller implementation files are generated.
     */
    @Parameter(property = SERVER_PREFIX + "controller.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".controller.package.name", required = true)
    protected String controllerPackageName;

    /**
     * Whether to generate authentication annotations for APIs.
     */
    @Parameter(property = SERVER_PREFIX + "use.auth", defaultValue = "false")
    protected boolean useAuth;

    /**
     * Determines if the server should use lombok.
     *
     * @since 4.2.2
     */
    @Parameter(property = SERVER_PREFIX + "lombok")
    protected boolean lombok;

    /**
     * Determines if the server should use flux for arrays.
     *
     * @since 4.2.2
     */
    @Parameter(property = SERVER_PREFIX + "flux.for.arrays")
    protected boolean fluxForArrays;

    /**
     * If set to true, the `javax.annotation.Generated` annotation will be added to all generated classes.
     *
     * @since 4.2.2
     */
    @Parameter(property = SERVER_PREFIX + "generated.annotation", defaultValue = "true")
    protected boolean generatedAnnotation;

    /**
     * If set to true, the generated code should be made compatible with Micronaut AOT.
     *
     * @since 4.2.2
     */
    @Parameter(property = SERVER_PREFIX + "aot.compatible")
    protected boolean aotCompatible;

    /**
     * The property that defines if this mojo should be used in configuration.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generate.server")
    protected boolean enabled;

    @Override
    protected boolean isEnabled() {
        return enabled;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorBuilder builder) {
        if ("kotlin".equalsIgnoreCase(lang)) {
            builder.forKotlinServer(spec -> spec
                .withControllerPackage(controllerPackageName)
                .withAuthentication(useAuth)
                .withAot(aotCompatible)
                // we don't want these to be configurable in the plugin for now
                .withGenerateImplementationFiles(false)
                .withGenerateControllerFromExamples(false)
                .withGenerateOperationsToReturnNotImplemented(false)
                .withGeneratedAnnotation(generatedAnnotation)
                .withFluxForArrays(fluxForArrays)
                .withKsp(ksp)
            );
        } else if ("java".equalsIgnoreCase(lang)) {
            builder.forJavaServer(spec -> spec
                .withControllerPackage(controllerPackageName)
                .withAuthentication(useAuth)
                .withAot(aotCompatible)
                // we don't want these to be configurable in the plugin for now
                .withGenerateImplementationFiles(false)
                .withGenerateControllerFromExamples(false)
                .withGenerateOperationsToReturnNotImplemented(false)
                .withGeneratedAnnotation(generatedAnnotation)
                .withFluxForArrays(fluxForArrays)
                .withLombok(lombok)
            );
        } else {
            throw new UnsupportedOperationException("Unsupported language: " + lang);
        }
    }
}
