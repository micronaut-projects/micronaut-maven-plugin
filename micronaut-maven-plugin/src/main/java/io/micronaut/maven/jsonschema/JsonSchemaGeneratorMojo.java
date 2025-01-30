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
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import io.micronaut.jsonschema.generator.SourceGenerator;
import io.micronaut.jsonschema.generator.utils.SourceGeneratorConfigBuilder;
import io.micronaut.jsonschema.generator.loaders.UrlLoader;

/**
 * Base class for Json Schema generator mojos. This provides the common
 * parameters for all generators and the invoker logic.
 */
@Mojo(name = JsonSchemaGeneratorMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public abstract class JsonSchemaGeneratorMojo extends AbstractMicronautMojo {
    public static final String MOJO_NAME = "generate-jsonschema";

    static final String MICRONAUT_SCHEMA_PREFIX = "micronaut.jsonschema.generator";
    static final String IO_MICRONAUT_SCHEMA_PREFIX = "io.micronaut.jsonschema";

    /**
     * The URL to an input resource, pointing to a JSON schema.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-url")
    protected String inputURL;

    /**
     * The input file containing the schema.
     * This file will be used as a source for generating or processing schemas.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-file")
    protected File inputFile;

    /**
     * The directory containing multiple input files or schema files.
     * The Mojo will process all schema files in this directory.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-folder")
    protected Path inputDirectory;

    /**
     * The programming language to be used for schema generation. Default is "JAVA".
     * Other values may be supported depending on the version of micronaut-sourcegen module.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".language", defaultValue = "JAVA")
    protected String language;

    /**
     * The output directory where generated sources or files will be placed.
     * By default, this points to `${project.build.directory}/generated-sources/jsonschema`.
     */
    @Parameter(defaultValue = "${project.build.directory}/generated-sources/jsonschema")
    protected File outputDirectory;

    /**
     * The package name for the generated classes or schemas.
     * Default value is specified as "io.micronaut.jsonschema".
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-package-name", defaultValue = IO_MICRONAUT_SCHEMA_PREFIX)
    protected String outputPackageName;

    /**
     * The name of the output file where the generated schema or data will be saved only if there is a single output source.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-file-name")
    protected String outputFileName;

    /**
     * A list of accepted URL patterns. Used to filter or validate input resources
     * based on their URL. URLs matching at least one pattern will be accepted.
     * Default value is "^https://.* /.*.json".
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".acceptedUrlPatterns")
    protected List<String> acceptedUrlPatterns;

    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    private Path getSourceDirectory(String language) {
        return outputDirectory.toPath().resolve("src/main/" + language.toLowerCase());
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        var langGenerator = new SourceGenerator(language.toUpperCase());

        if (acceptedUrlPatterns != null && !acceptedUrlPatterns.isEmpty()) {
            UrlLoader.addAllowedUrlPatterns(acceptedUrlPatterns);
        }

        Path outputDirPath = getSourceDirectory(language);
        project.addCompileSourceRoot(outputDirPath.toString());

        var builder = new SourceGeneratorConfigBuilder()
                .withOutputFolder(outputDirPath)
                .withOutputPackageName(outputPackageName)
                .withOutputFileName(outputFileName);

        if (inputURL != null) {
            builder.withJsonUrl(inputURL);
        } else if (inputFile != null) {
            builder.withJsonFile(inputFile);
        } else if (inputDirectory != null) {
            builder.withInputFolder(inputDirectory);
        } else {
            throw new MojoExecutionException("In the generate-jsonschema mojo, " +
                    "one of the following parameters needs to be specified: \".input-url\", \".input-file\", or \".input-folder\"");
        }

        try {
            langGenerator.generate(builder.build());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
