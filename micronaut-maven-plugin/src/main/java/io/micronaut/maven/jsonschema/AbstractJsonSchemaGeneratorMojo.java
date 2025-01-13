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

import io.micronaut.maven.AbstractMicronautMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.List;

import io.micronaut.jsonschema.generator.SourceGenerator;
import io.micronaut.jsonschema.generator.utils.SourceGeneratorConfigBuilder;
import io.micronaut.jsonschema.generator.loaders.UrlLoader;

/**
 * Base class for Json Schema generator mojos. This provides the common
 * parameters for all generators and the invoker logic.
 */
public abstract class AbstractJsonSchemaGeneratorMojo extends AbstractMicronautMojo {
    static final String MICRONAUT_SCHEMA_PREFIX = "micronaut.jsonschema.generator";
    static final String IO_MICRONAUT_OPENAPI_PREFIX = "io." + MICRONAUT_SCHEMA_PREFIX;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".language", defaultValue = "JAVA")
    protected String language;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-folder", required = true)
    protected File outputDirectory;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-package-name")
    protected String outputPackageName;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-file-name")
    protected String outputFileName;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".acceptedUrlPatterns")
    protected List<String> acceptedUrlPatterns;

    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    protected abstract void configureBuilder(SourceGeneratorConfigBuilder builder);

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        var langGenerator = new SourceGenerator(language.toUpperCase());

        if (!acceptedUrlPatterns.isEmpty()) {
            UrlLoader.setAllowedUrlPatterns(acceptedUrlPatterns);
        }

        var builder = SourceGeneratorConfigBuilder
                .withOutputFolder(outputDirectory.getAbsolutePath())
                .withOutputPackageName(outputPackageName)
                .withOutputFileName(outputFileName);
        configureBuilder(builder);
        langGenerator.generate(builder.build());
    }
}
