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

    /**
     *
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".client.id", defaultValue = "")
    protected String clientId;

    /**
     * Whether to configure authentication for client.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".client.use.auth", defaultValue = "false")
    protected boolean useAuth;

    /**
     * Additional annotations to be used on the generated client API classes.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".client.additional.type.annotations")
    protected List<String> additionalTypeAnnotations;

    /**
     * The base path separator.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".client.base.path.separator")
    protected String basePathSeparator;

    /**
     * The pattern for authorization filter.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".client.authorization.filter.pattern")
    protected String authorizationFilterPattern;

    /**
     * The property that defines if this mojo is used.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generate.client")
    protected boolean enabled;

    @Override
    protected boolean isEnabled() {
        return enabled;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorBuilder builder) {
        builder.forClient(spec -> {
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
        });
    }
}
