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
package io.micronaut.maven.jsonschema;

import io.micronaut.jsonschema.generator.utils.SourceGeneratorConfigBuilder;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Generates sources from a valid URL of a schema.
 * The sources are generated in the target directory.
 */
@Mojo(name = JsonSchemaUrlMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public abstract class JsonSchemaUrlMojo extends AbstractJsonSchemaGeneratorMojo {
    public static final String MOJO_NAME = "generate-jsonschema-from-url";

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-url", required = true)
    protected String inputURL;

    @Override
    protected void configureBuilder(SourceGeneratorConfigBuilder builder) {
        builder.withJsonUrl(inputURL);
    }
}
