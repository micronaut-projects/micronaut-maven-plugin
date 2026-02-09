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

import java.io.File;
import java.util.List;

/**
 * Shared configuration for Micronaut configuration validation.
 * <p>
 * This type is used as a nested configuration object in multiple mojos.
 */
public final class ConfigurationValidationConfiguration {

    /**
     * Global enable/disable flag.
     */
    private Boolean enabled;

    /**
     * Suppression patterns (same as the CLI --suppress/--suppressions flags).
     */
    private List<String> suppressions;

    /**
     * Whether to fail on unknown / not-present properties.
     */
    private Boolean failOnNotPresent;

    /**
     * Whether to allow Micronaut to deduce environments.
     */
    private Boolean deduceEnvironments;

    /**
     * Report format: json|html|both.
     */
    private String format;

    /**
     * Base output directory for reports.
     */
    private File outputDirectory;

    /**
     * Enable the on-disk cache to avoid re-running validation when inputs have not changed.
     */
    private Boolean cacheEnabled;

    /**
     * If {@code true}, cache invalidation only considers changes under src/main/resources.
     */
    private Boolean cacheMainResourcesOnly;

    private ValidationSet dev;
    private ValidationSet packageValidation;
    private ValidationSet test;

    /**
     * @return Whether configuration validation is enabled. If {@code null}, validation is enabled.
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * @param enabled Whether configuration validation is enabled.
     */
    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Suppression patterns applied to validation results.
     *
     * @return The suppression patterns, or {@code null} to use no suppressions.
     */
    public List<String> getSuppressions() {
        return suppressions;
    }

    /**
     * @param suppressions Suppression patterns.
     */
    public void setSuppressions(List<String> suppressions) {
        this.suppressions = suppressions;
    }

    /**
     * Controls whether validation should fail when a configuration property is not present in the schema.
     *
     * @return Whether to fail on not-present properties. If {@code null}, defaults to {@code true}.
     */
    public Boolean getFailOnNotPresent() {
        return failOnNotPresent;
    }

    /**
     * @param failOnNotPresent Whether to fail on not-present properties.
     */
    public void setFailOnNotPresent(Boolean failOnNotPresent) {
        this.failOnNotPresent = failOnNotPresent;
    }

    /**
     * Controls whether Micronaut may deduce environments.
     *
     * @return Whether to deduce environments. If {@code null}, defaults to {@code false}.
     */
    public Boolean getDeduceEnvironments() {
        return deduceEnvironments;
    }

    /**
     * @param deduceEnvironments Whether to allow Micronaut to deduce environments.
     */
    public void setDeduceEnvironments(Boolean deduceEnvironments) {
        this.deduceEnvironments = deduceEnvironments;
    }

    /**
     * Report format.
     *
     * @return The report format ({@code json}, {@code html}, or {@code both}). If {@code null}, defaults to {@code both}.
     */
    public String getFormat() {
        return format;
    }

    /**
     * @param format The report format ({@code json}, {@code html}, or {@code both}).
     */
    public void setFormat(String format) {
        this.format = format;
    }

    /**
     * Base output directory for reports.
     *
     * @return The base output directory. If {@code null}, defaults to
     * {@code ${project.build.directory}/micronaut/config-validation}.
     */
    public File getOutputDirectory() {
        return outputDirectory;
    }

    /**
     * @param outputDirectory The base output directory.
     */
    public void setOutputDirectory(File outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    /**
     * @return Whether caching is enabled. If {@code null}, defaults to {@code true}.
     */
    public Boolean getCacheEnabled() {
        return cacheEnabled;
    }

    /**
     * @param cacheEnabled Whether caching is enabled.
     */
    public void setCacheEnabled(Boolean cacheEnabled) {
        this.cacheEnabled = cacheEnabled;
    }

    /**
     * Controls whether cache invalidation only considers changes under {@code src/main/resources}.
     *
     * @return Whether cache invalidation is based on {@code src/main/resources}. If {@code null}, defaults to {@code true}.
     */
    public Boolean getCacheMainResourcesOnly() {
        return cacheMainResourcesOnly;
    }

    /**
     * @param cacheMainResourcesOnly Whether cache invalidation only considers changes under {@code src/main/resources}.
     */
    public void setCacheMainResourcesOnly(Boolean cacheMainResourcesOnly) {
        this.cacheMainResourcesOnly = cacheMainResourcesOnly;
    }

    /**
     * @return Scenario-specific configuration for {@code mn:run} validation (default environment: {@code dev}).
     */
    public ValidationSet getDev() {
        return dev;
    }

    /**
     * @param dev Scenario-specific configuration for {@code mn:run} validation.
     */
    public void setDev(ValidationSet dev) {
        this.dev = dev;
    }

    /**
     * @return Scenario-specific configuration for {@code package} validation.
     */
    public ValidationSet getPackageValidation() {
        return packageValidation;
    }

    /**
     * @param packageValidation Scenario-specific configuration for {@code package} validation.
     */
    public void setPackageValidation(ValidationSet packageValidation) {
        this.packageValidation = packageValidation;
    }

    /**
     * @return Scenario-specific configuration for {@code test} validation (default environment: {@code test}).
     */
    public ValidationSet getTest() {
        return test;
    }

    /**
     * @param test Scenario-specific configuration for {@code test} validation.
     */
    public void setTest(ValidationSet test) {
        this.test = test;
    }

    /**
     * Scenario-specific configuration.
     */
    public static final class ValidationSet {
        /**
         * Enables/disables validation for this scenario.
         */
        private Boolean enabled;

        /**
         * Environments to validate.
         */
        private List<String> environments;

        /**
         * Whether to include the scenario's default environment(s).
         */
        private Boolean includeDefaultEnvironment;

        /**
         * Explicit classpath elements to validate (replaces defaults).
         */
        private List<String> classpathElements;

        /**
         * Additional classpath entries appended to the computed defaults.
         */
        private List<String> additionalClasspathElements;

        /**
         * Override output directory for this scenario.
         */
        private File outputDirectory;

        /**
         * @return Whether this validation scenario is enabled. If {@code null}, the scenario is enabled.
         */
        public Boolean getEnabled() {
            return enabled;
        }

        /**
         * @param enabled Whether this validation scenario is enabled.
         */
        public void setEnabled(Boolean enabled) {
            this.enabled = enabled;
        }

        /**
         * @return The environments to validate for this scenario. These environments are appended to the scenario defaults
         * (unless {@link #getIncludeDefaultEnvironment()} is {@code false}).
         */
        public List<String> getEnvironments() {
            return environments;
        }

        /**
         * @param environments The environments to validate.
         */
        public void setEnvironments(List<String> environments) {
            this.environments = environments;
        }

        /**
         * @return Whether to include the scenario's default environments. If {@code null}, defaults to {@code true}.
         */
        public Boolean getIncludeDefaultEnvironment() {
            return includeDefaultEnvironment;
        }

        /**
         * @param includeDefaultEnvironment Whether to include the scenario's default environments.
         */
        public void setIncludeDefaultEnvironment(Boolean includeDefaultEnvironment) {
            this.includeDefaultEnvironment = includeDefaultEnvironment;
        }

        /**
         * Full classpath to validate for this scenario.
         *
         * @return Classpath elements that replace the plugin-computed defaults. If {@code null} or empty, the plugin
         * computes defaults for the scenario.
         */
        public List<String> getClasspathElements() {
            return classpathElements;
        }

        /**
         * @param classpathElements Classpath elements that replace the plugin-computed defaults.
         */
        public void setClasspathElements(List<String> classpathElements) {
            this.classpathElements = classpathElements;
        }

        /**
         * @return Additional classpath elements appended to the plugin-computed defaults.
         */
        public List<String> getAdditionalClasspathElements() {
            return additionalClasspathElements;
        }

        /**
         * @param additionalClasspathElements Additional classpath elements appended to the plugin-computed defaults.
         */
        public void setAdditionalClasspathElements(List<String> additionalClasspathElements) {
            this.additionalClasspathElements = additionalClasspathElements;
        }

        /**
         * @return Scenario output directory override. If {@code null}, the plugin uses
         * {@code <baseOutputDirectory>/<scenario>}.
         */
        public File getOutputDirectory() {
            return outputDirectory;
        }

        /**
         * @param outputDirectory Scenario output directory override.
         */
        public void setOutputDirectory(File outputDirectory) {
            this.outputDirectory = outputDirectory;
        }
    }
}
