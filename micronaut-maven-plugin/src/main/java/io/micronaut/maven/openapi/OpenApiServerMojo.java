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

import io.micronaut.openapi.generator.MicronautCodeGeneratorEntryPoint;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Generates an OpenAPI server.
 * The sources are generated in the target directory.
 */
@Mojo(name = OpenApiServerMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class OpenApiServerMojo extends AbstractOpenApiMojo {
    public static final String MOJO_NAME = "openapi-server";

    @Parameter(property = "micronaut.openapi.server.controller.package.name", defaultValue = "io.micronaut.openapi.controller.package.name", required = true)
    protected String controllerPackageName;

    @Parameter(property = "micronaut.openapi.server.use.auth", defaultValue = "false")
    protected boolean useAuth;

    @Parameter(property = "micronaut.openapi.generate.server")
    protected boolean enabled;

    @Override
    protected boolean isEnabled() {
        return enabled;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorEntryPoint.Builder builder) {
        builder.forServer(spec -> {
            spec.withControllerPackage(controllerPackageName);
            spec.withAuthentication(useAuth);
            // we don't want these to be configurable in the plugin for now
            spec.withGenerateAbstractClasses(true);
            spec.withGenerateControllerFromExamples(false);
            spec.withGenerateOperationsToReturnNotImplemented(false);
        });
    }
}
