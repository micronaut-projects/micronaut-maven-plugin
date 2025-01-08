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
import io.micronaut.openapi.generator.MicronautCodeGeneratorEntryPoint;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for Json Schema generator mojos. This provides the common
 * parameters for all generators and the invoker logic.
 */
public abstract class JsonSchemaGeneratorMojo extends AbstractMicronautMojo {
    static final String MICRONAUT_SCHEMA_PREFIX = "micronaut.jsonschema.generator";
    static final String IO_MICRONAUT_OPENAPI_PREFIX = "io." + MICRONAUT_SCHEMA_PREFIX;

    // TODO: Classpath?
    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-url")
    protected String inputURL;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-file")
    protected File inputFile;

    @Parameter(property = MICRONAUT_SCHEMA_PREFIX + ".input-folder")
    protected File inputDirectory;

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

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        var builder = MicronautCodeGeneratorEntryPoint.builder()
                .withDefinitionFile(inputFile.toURI())
                .withOutputDirectory(outputDirectory)
                .withOutputs();

//        var generatedSourcesDir = getGeneratedSourcesDirectory().get().getAsFile();
//        var lang = getLanguage().getOrElse("java");
//        String jsonURL = getJsonURL().isPresent() ? getJsonURL().get() : "";
//        String jsonFile = getJsonFile().isPresent() ? getJsonFile().get().getAsFile().toURI().toString() : "";
//        String inputPath = getInputDirectory().isPresent() ? getInputDirectory().get().getAsFile().getAbsolutePath() : "";
//        if (jsonURL.isBlank() && jsonFile.isBlank() && inputPath.isBlank()) {
//            throw new TaskInstantiationException("One of the arguments needs to be provided: jsonURL, jsonFile or inputDirectory.");
//        }
//
//        Files.createDirectories(generatedSourcesDir.toPath());
//        getExecOperations().javaexec(javaexec -> {
//            javaexec.setClasspath(getClasspath());
//            javaexec.getMainClass().set("io.micronaut.jsonschema.generator.GeneratorMain");
//            var args = new ArrayList<String>();
//            args.add(jsonURL);
//            args.add(jsonFile);
//            args.add(inputPath);
//            args.add(lang.toUpperCase());
//            args.add(getGeneratedSourcesDirectory().get().getAsFile().getAbsolutePath());
//            args.add(getPackageName().getOrElse(""));
//            args.add(getOutputFileName().getOrElse(""));
//            args.add(getAcceptedUrlPatterns().getOrElse(List.of("")).toString());
//            javaexec.args(args);
//        });

        builder.build().generate();
    }
}
