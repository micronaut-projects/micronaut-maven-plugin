/*
 * Copyright 2017-2026 original authors
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

import io.micronaut.json.JsonMapper;
import io.micronaut.jsonschema.configuration.validator.ConfigurationError;
import io.micronaut.jsonschema.configuration.validator.ConfigurationJsonSchemaValidator;
import io.micronaut.jsonschema.configuration.validator.cli.JsonSchemaConfigurationValidator;
import io.micronaut.jsonschema.configuration.validator.report.HtmlConfigurationErrorReporter;
import io.micronaut.jsonschema.configuration.validator.report.JsonConfigurationErrorReporter;
import io.micronaut.jsonschema.configuration.validator.report.SystemErrConfigurationErrorReporter;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

/**
 * Executes Micronaut configuration validation using the Micronaut JSON Schema configuration validator.
 */
final class ConfigurationValidationExecutor {
    private ConfigurationValidationExecutor() {
    }

    /**
     * Validate configuration and write reports.
     *
     * @param classpath Path-separator separated classpath to validate
     * @param environments Environments to enable
     * @param suppressions Suppression patterns
     * @param failOnNotPresent Whether to fail when properties are not present in schema
     * @param deduceEnvironments Whether to allow Micronaut to deduce environments
     * @param outputDir Output directory
     * @param format Report format
     * @param projectBaseDir Project base directory for relative path resolution in error reports
     * @param resourcesDirs Resource directories used to resolve and render relative origin paths in error output
     * @param err Error stream for reporting
     * @return The validation result
     * @throws IOException If validation or report writing fails
     */
    static ValidationResult validate(
        String classpath,
        List<String> environments,
        List<String> suppressions,
        boolean failOnNotPresent,
        boolean deduceEnvironments,
        Path outputDir,
        ConfigurationValidationFormat format,
        Path projectBaseDir,
        List<Path> resourcesDirs,
        PrintStream err
    ) throws IOException {
        Files.createDirectories(outputDir);

        ConfigurationJsonSchemaValidator validator = new ConfigurationJsonSchemaValidator();
        validator.setFailOnNotPresent(failOnNotPresent);
        validator.setSuppressionPatterns(suppressions);

        JsonSchemaConfigurationValidator facade = JsonSchemaConfigurationValidator.forClasspath(
            classpath,
            environments,
            deduceEnvironments,
            validator
        );

        Set<ConfigurationError> errors = facade.validate();

        Path jsonFile = null;
        Path htmlFile = null;

        if (format == ConfigurationValidationFormat.JSON || format == ConfigurationValidationFormat.BOTH) {
            jsonFile = outputDir.resolve("configuration-errors.json");
            try (OutputStream os = Files.newOutputStream(jsonFile)) {
                new JsonConfigurationErrorReporter(JsonMapper.createDefault(), os).report(errors);
            }
        }
        if (format == ConfigurationValidationFormat.HTML || format == ConfigurationValidationFormat.BOTH) {
            htmlFile = outputDir.resolve("configuration-errors.html");
            try (OutputStream os = Files.newOutputStream(htmlFile)) {
                new HtmlConfigurationErrorReporter(os).report(errors);
            }
        }

        new SystemErrConfigurationErrorReporter(err, htmlFile, jsonFile, projectBaseDir, resourcesDirs).report(errors);

        boolean hasErrors = errors.stream().anyMatch(e -> e.type() == ConfigurationError.Type.ERROR);
        return new ValidationResult(errors, hasErrors, outputDir.toFile());
    }

    /**
     * Result of a validation run.
     *
     * @param errors All errors and warnings
     * @param hasErrors Whether any error-level entries are present
     * @param outputDirectory Output directory where reports were written
     */
    record ValidationResult(Set<ConfigurationError> errors, boolean hasErrors, File outputDirectory) {
    }
}
