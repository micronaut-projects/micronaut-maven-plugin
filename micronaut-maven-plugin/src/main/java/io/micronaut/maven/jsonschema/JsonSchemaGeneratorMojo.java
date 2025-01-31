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

import io.micronaut.core.util.StringUtils;
import io.micronaut.maven.AbstractMicronautMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import io.micronaut.jsonschema.generator.SourceGenerator;
import io.micronaut.jsonschema.generator.utils.SourceGeneratorConfigBuilder;
import io.micronaut.jsonschema.generator.loaders.UrlLoader;

import javax.inject.Inject;

/**
 * Json Schema generator mojo provides the parameters for all generators and the invoker logic.
 * <p>
 * Expects single or multiple schema files as input via a URL, file, or directory;
 * and generates all required source code representing the validation form in the targeted language.
 * </p>
 */
@Mojo(name = JsonSchemaGeneratorMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class JsonSchemaGeneratorMojo extends AbstractMicronautMojo {
    public static final String MOJO_NAME = "generate-jsonschema";

    static final String MICRONAUT_SCHEMA_PREFIX = "micronaut.jsonschema.generator";
    static final String IO_MICRONAUT_SCHEMA_PREFIX = "io.micronaut.jsonschema";

    /**
     * The URL to an input resource, pointing to a JSON schema.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-url")
    private String inputURL;

    /**
     * The input file containing the schema.
     * This file will be used as a source for generating or processing schemas.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-file")
    private File inputFile;

    /**
     * The directory containing multiple input files or schema files.
     * The Mojo will process all schema files in this directory.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-directory")
    private File inputDirectory;

    /**
     * The programming language to be used for schema generation. Default is "JAVA".
     * Other values may be supported depending on the version of micronaut-sourcegen module.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".language", defaultValue = "JAVA")
    private String language;

    /**
     * The output directory where generated sources or files will be placed.
     * By default, this points to `${project.build.directory}/generated-sources/jsonschema`.
     */
    @Parameter(defaultValue = "${project.build.directory}/generated-sources/jsonschema")
    private File outputDirectory;

    /**
     * The package name for the generated classes or schemas.
     * Default value is specified as "io.micronaut.jsonschema".
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-package-name", defaultValue = IO_MICRONAUT_SCHEMA_PREFIX)
    private String outputPackageName;

    /**
     * The name of the output file where the generated schema or data will be saved only if there is a single output source.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".output-file-name")
    private String outputFileName;

    /**
     * A list of accepted URL patterns. Used to filter or validate input resources
     * based on their URL. URLs matching at least one pattern will be accepted.
     * Default value is "^https://.* /.*.json".
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".accepted-url-patterns")
    private List<String> acceptedUrlPatterns;

    /**
     * The property that defines if this mojo is used.
     */
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".enabled", defaultValue = StringUtils.FALSE)
    private boolean enabled;

    private final MavenProject project;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public JsonSchemaGeneratorMojo(final MavenProject project) {
        this.project = project;
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (!enabled) {
            if (getLog().isDebugEnabled()) {
                getLog().debug(MOJO_NAME + " is disabled");
            }
            return;
        }

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

        var message = "Generating sources for JSON schema from %s in the directory: %s";
        var relativePath = relativize(outputDirPath);
        if (inputURL != null) {
            builder.withJsonUrl(inputURL);
            message = message.formatted("URL [" + inputURL + "]", relativePath);
        } else if (inputFile != null) {
            builder.withJsonFile(inputFile);
            message = message.formatted("file [" + relativize(inputFile.toPath()) + "]", relativePath);
        } else if (inputDirectory != null) {
            builder.withInputFolder(inputDirectory.toPath());
            message = message.formatted("directory [" + relativize(inputDirectory.toPath()) + "]", relativePath);
        } else {
            var msg = new StringBuilder("In the generate-jsonschema goal, one of the following parameters needs to be specified:")
                    .append(System.lineSeparator())
                    .append("%s.input-file".formatted(MICRONAUT_SCHEMA_PREFIX))
                    .append(System.lineSeparator())
                    .append("%s.input-url".formatted(MICRONAUT_SCHEMA_PREFIX))
                    .append(System.lineSeparator())
                    .append("%s.input-directory".formatted(MICRONAUT_SCHEMA_PREFIX))
                    .append(System.lineSeparator());
            throw new MojoFailureException(msg.toString());
        }

        try {
            getLog().info(message);
            langGenerator.generate(builder.build());
        } catch (Exception e) {
            throw new MojoExecutionException("Error when generating JSON schema", e);
        }
    }

    private String relativize(Path path) {
        return project.getBasedir().toPath().relativize(path).toString();
    }

    private Path getSourceDirectory(String language) {
        return outputDirectory.toPath().resolve("src/main/" + language.toLowerCase());
    }
}
