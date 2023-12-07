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

import java.util.List;

/**
 * Generates an OpenAPI client.
 * The sources are generated in the target directory.
 */
@Mojo(name = OpenApiClientMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class OpenApiClientMojo extends AbstractOpenApiMojo {
    public static final String MOJO_NAME = "generate-openapi-client";
    private static final String CLIENT_PREFIX = MICRONAUT_OPENAPI_PREFIX + ".client.";

    /**
     * Client id.
     */
    @Parameter(property = CLIENT_PREFIX  + "id", defaultValue = "")
    protected String clientId;

    /**
     * Whether to configure authentication for client.
     */
    @Parameter(property = CLIENT_PREFIX + "use.auth", defaultValue = "false")
    protected boolean useAuth;

    /**
     * Additional annotations to be used on the generated client API classes.
     */
    @Parameter(property = CLIENT_PREFIX + "additional.type.annotations")
    protected List<String> additionalTypeAnnotations;

    /**
     * The base path separator.
     */
    @Parameter(property = CLIENT_PREFIX + "base.path.separator")
    protected String basePathSeparator;

    /**
     * The pattern for authorization filter.
     */
    @Parameter(property = CLIENT_PREFIX + "authorization.filter.pattern")
    protected String authorizationFilterPattern;

    /**
     * The property that defines if this mojo is used.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generate.client")
    protected boolean enabled;

    /**
     * Determines if the client should use lombok.
     */
    @Parameter(property = CLIENT_PREFIX + "lombok")
    protected boolean lombok;

    /**
     * Determines if the client should use flux for arrays.
     */
    @Parameter(property = CLIENT_PREFIX + "flux.for.arrays")
    protected boolean fluxForArrays;

    /**
     * If set to true, the `javax.annotation.Generated` annotation will be added to all generated classes.
     */
    @Parameter(property = CLIENT_PREFIX + "generated.annotation", defaultValue = "true")
    protected boolean generatedAnnotation;

    @Override
    protected boolean isEnabled() {
        return enabled;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorBuilder builder) {
        builder.forJavaClient(spec -> {
            spec.withAuthorization(useAuth);
            if (clientId != null && !clientId.isEmpty()) {
                spec.withClientId(clientId);
            }
            if (additionalTypeAnnotations != null) {
                spec.withAdditionalClientTypeAnnotations(additionalTypeAnnotations);
            }
            if (basePathSeparator != null) {
                spec.withBasePathSeparator(basePathSeparator);
            }
            if (authorizationFilterPattern != null) {
                spec.withAuthorizationFilterPattern(authorizationFilterPattern);
            }
            spec.withLombok(lombok);
            spec.withFluxForArrays(fluxForArrays);
            spec.withGeneratedAnnotation(generatedAnnotation);
        });
    }
}
