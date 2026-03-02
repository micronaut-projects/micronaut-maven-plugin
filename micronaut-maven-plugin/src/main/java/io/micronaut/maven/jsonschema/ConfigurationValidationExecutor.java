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
import io.micronaut.jsonschema.configuration.validator.DependencyInjectionError;
import io.micronaut.jsonschema.configuration.validator.cli.DependencyInjectionConfigurationValidator;
import io.micronaut.jsonschema.configuration.validator.cli.JsonSchemaConfigurationValidator;
import io.micronaut.jsonschema.configuration.validator.report.HtmlConfigurationErrorReporter;
import io.micronaut.jsonschema.configuration.validator.report.JsonConfigurationErrorReporter;
import io.micronaut.jsonschema.configuration.validator.report.SystemErrConfigurationErrorReporter;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

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
     * @param suppressInjectErrors Dependency-injection suppression patterns
     * @param failOnNotPresent Whether to fail when properties are not present in schema
     * @param deduceEnvironments Whether to allow Micronaut to deduce environments
     * @param validateDependencyInjection Whether to validate dependency injection
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
        List<String> suppressInjectErrors,
        boolean failOnNotPresent,
        boolean deduceEnvironments,
        boolean validateDependencyInjection,
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
        Set<DependencyInjectionError> dependencyInjectionErrors = Set.of();

        if (validateDependencyInjection) {
            dependencyInjectionErrors = validateDependencyInjection(
                classpath,
                environments,
                deduceEnvironments,
                suppressInjectErrors
            );
        }

        Path jsonFile = null;
        Path htmlFile = null;

        if (format == ConfigurationValidationFormat.JSON || format == ConfigurationValidationFormat.BOTH) {
            jsonFile = outputDir.resolve("configuration-errors.json");
            try (OutputStream os = Files.newOutputStream(jsonFile)) {
                new JsonConfigurationErrorReporter(JsonMapper.createDefault(), os).report(errors, dependencyInjectionErrors);
            }
        }
        if (format == ConfigurationValidationFormat.HTML || format == ConfigurationValidationFormat.BOTH) {
            htmlFile = outputDir.resolve("configuration-errors.html");
            try (OutputStream os = Files.newOutputStream(htmlFile)) {
                new HtmlConfigurationErrorReporter(os).report(errors, dependencyInjectionErrors);
            }
        }

        new SystemErrConfigurationErrorReporter(err, htmlFile, jsonFile, projectBaseDir, resourcesDirs).report(errors, dependencyInjectionErrors);

        boolean hasErrors = errors.stream().anyMatch(e -> e.type() == ConfigurationError.Type.ERROR);
        hasErrors = hasErrors || !dependencyInjectionErrors.isEmpty();
        return new ValidationResult(errors, hasErrors, outputDir.toFile());
    }

    private static Set<DependencyInjectionError> validateDependencyInjection(
        String classpath,
        List<String> environments,
        boolean deduceEnvironments,
        List<String> suppressInjectErrors
    ) {
        Set<DependencyInjectionError> errors = validateWithSuppressionAwareValidator(
            classpath,
            environments,
            deduceEnvironments,
            suppressInjectErrors
        );
        if (errors != null) {
            return errors;
        }
        Set<DependencyInjectionError> legacyErrors = DependencyInjectionConfigurationValidator.forClasspath(
            classpath,
            environments,
            deduceEnvironments
        ).validate();
        return applyLegacySuppressions(legacyErrors, suppressInjectErrors);
    }

    @SuppressWarnings("unchecked")
    private static Set<DependencyInjectionError> validateWithSuppressionAwareValidator(
        String classpath,
        List<String> environments,
        boolean deduceEnvironments,
        List<String> suppressInjectErrors
    ) {
        try {
            Method forClasspath = DependencyInjectionConfigurationValidator.class.getMethod(
                "forClasspath",
                String.class,
                List.class,
                boolean.class,
                List.class
            );
            Object validator = forClasspath.invoke(null, classpath, environments, deduceEnvironments, suppressInjectErrors);
            return ((DependencyInjectionConfigurationValidator) validator).validate();
        } catch (NoSuchMethodException e) {
            return null;
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new IllegalStateException("Dependency injection validation failed", e);
        }
    }

    private static Set<DependencyInjectionError> applyLegacySuppressions(
        Set<DependencyInjectionError> errors,
        List<String> suppressInjectErrors
    ) {
        if (suppressInjectErrors.isEmpty() || errors.isEmpty()) {
            return errors;
        }
        List<Pattern> patterns = compileSuppressionPatterns(suppressInjectErrors);
        if (patterns.isEmpty()) {
            return errors;
        }
        return errors.stream()
            .filter(error -> !isSuppressed(error, patterns))
            .collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
    }

    private static List<Pattern> compileSuppressionPatterns(List<String> suppressInjectErrors) {
        List<Pattern> patterns = new ArrayList<>(suppressInjectErrors.size());
        for (String rawPattern : suppressInjectErrors) {
            if (rawPattern == null || rawPattern.isBlank()) {
                continue;
            }
            String pattern = rawPattern.trim();
            if (pattern.indexOf('*') > -1) {
                patterns.add(Pattern.compile(wildcardToRegex(pattern)));
            } else {
                patterns.add(Pattern.compile("^" + Pattern.quote(pattern) + "$"));
            }
        }
        return patterns;
    }

    private static boolean isSuppressed(DependencyInjectionError error, List<Pattern> patterns) {
        return matches(error.rootBean(), patterns)
            || matches(error.bean(), patterns)
            || matches(error.injectionPoint(), patterns);
    }

    private static boolean matches(String value, List<Pattern> patterns) {
        if (value == null || value.isBlank()) {
            return false;
        }
        for (Pattern pattern : patterns) {
            if (pattern.matcher(value).find()) {
                return true;
            }
        }
        return false;
    }

    private static String wildcardToRegex(String wildcardPattern) {
        StringBuilder out = new StringBuilder(wildcardPattern.length() + 16);
        out.append('^');
        for (int i = 0; i < wildcardPattern.length(); i++) {
            char c = wildcardPattern.charAt(i);
            if (c == '*') {
                out.append(".*");
            } else if ("\\.[]{}()+-^$|?".indexOf(c) >= 0) {
                out.append('\\').append(c);
            } else {
                out.append(c);
            }
        }
        out.append('$');
        return out.toString();
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
