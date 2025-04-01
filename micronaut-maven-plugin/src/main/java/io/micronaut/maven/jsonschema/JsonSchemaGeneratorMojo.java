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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import java.io.File;
import java.nio.file.Path;

/**
 * Json Schema generator mojo provides the parameters for all generators and the invoker logic.
 * <p>
 * Expects single or multiple schema files as input via a URL, file, or directory;
 * and generates all required source code representing the validation form in the targeted language.
 * </p>
 */
@Mojo(name = JsonSchemaGeneratorMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class JsonSchemaGeneratorMojo extends AbstractJsonSchemaGeneratorMojo {

    public static final String MOJO_NAME = "generate-jsonschema";

    static final String MICRONAUT_SCHEMA_PREFIX = "micronaut.jsonschema.generator";

    /**
     * The output directory where generated sources or files will be placed.
     * By default, this points to `${project.build.directory}/generated-sources/jsonschema`.
     */
    @Parameter(defaultValue = "${project.build.directory}/generated-sources/jsonschema")
    private File outputDirectory;

    @Override
    protected File getJavaOutputDirectory() {
        return new File(outputDirectory, "src/main/java");
    }

    @Override
    protected File getGroovyOutputDirectory() {
        return new File(outputDirectory, "src/main/groovy");
    }

    @Override
    protected File getKotlinOutputDirectory() {
        return new File(outputDirectory, "src/main/kotlin");
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (!enabled) {
            if (getLog().isDebugEnabled()) {
                getLog().debug(MOJO_NAME + " is disabled");
            }
            return;
        }

        var message = "Generating sources for JSON schema from %s in the directory: %s";
        var relativePath = relativize(outputDirectory.toPath());
        if (inputURL != null) {
            message = message.formatted("URL [" + inputURL + "]", relativePath);
        } else if (inputFile != null) {
            message = message.formatted("file [" + relativize(inputFile.toPath()) + "]", relativePath);
        } else if (inputDirectory != null) {
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
        getLog().info(message);

        super.execute();
    }

    private String relativize(Path path) {
        return project.getBasedir().toPath().relativize(path).toString();
    }

}
