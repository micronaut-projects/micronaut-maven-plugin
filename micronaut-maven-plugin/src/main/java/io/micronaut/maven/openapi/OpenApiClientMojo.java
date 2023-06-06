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

import java.util.List;

/**
 * Generates an OpenAPI client.
 * The sources are generated in the target directory.
 */
@Mojo(name = OpenApiClientMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class OpenApiClientMojo extends AbstractOpenApiMojo {
    public static final String MOJO_NAME = "openapi-client";

    @Parameter(property = "micronaut.openapi.client.id", defaultValue = "")
    protected String clientId;

    @Parameter(property = "micronaut.openapi.client.use.auth", defaultValue = "false")
    protected boolean useAuth;

    @Parameter(property = "micronaut.openapi.client.additional.type.annotations")
    protected List<String> additionalTypeAnnotations;

    @Parameter(property = "micronaut.openapi.client.base.path.separator")
    protected String basePathSeparator;

    @Parameter(property = "micronaut.openapi.client.authorization.filter.pattern")
    protected String authorizationFilterPattern;

    @Parameter(property = "micronaut.openapi.generate.client")
    protected boolean enabled;

    @Override
    protected boolean isEnabled() {
        return enabled;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorEntryPoint.Builder builder) {
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
