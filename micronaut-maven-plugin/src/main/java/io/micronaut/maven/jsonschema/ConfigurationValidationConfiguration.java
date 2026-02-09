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

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public List<String> getSuppressions() {
        return suppressions;
    }

    public void setSuppressions(List<String> suppressions) {
        this.suppressions = suppressions;
    }

    public Boolean getFailOnNotPresent() {
        return failOnNotPresent;
    }

    public void setFailOnNotPresent(Boolean failOnNotPresent) {
        this.failOnNotPresent = failOnNotPresent;
    }

    public Boolean getDeduceEnvironments() {
        return deduceEnvironments;
    }

    public void setDeduceEnvironments(Boolean deduceEnvironments) {
        this.deduceEnvironments = deduceEnvironments;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public File getOutputDirectory() {
        return outputDirectory;
    }

    public void setOutputDirectory(File outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    public Boolean getCacheEnabled() {
        return cacheEnabled;
    }

    public void setCacheEnabled(Boolean cacheEnabled) {
        this.cacheEnabled = cacheEnabled;
    }

    public Boolean getCacheMainResourcesOnly() {
        return cacheMainResourcesOnly;
    }

    public void setCacheMainResourcesOnly(Boolean cacheMainResourcesOnly) {
        this.cacheMainResourcesOnly = cacheMainResourcesOnly;
    }

    public ValidationSet getDev() {
        return dev;
    }

    public void setDev(ValidationSet dev) {
        this.dev = dev;
    }

    public ValidationSet getPackageValidation() {
        return packageValidation;
    }

    public void setPackageValidation(ValidationSet packageValidation) {
        this.packageValidation = packageValidation;
    }

    public ValidationSet getTest() {
        return test;
    }

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

        public Boolean getEnabled() {
            return enabled;
        }

        public void setEnabled(Boolean enabled) {
            this.enabled = enabled;
        }

        public List<String> getEnvironments() {
            return environments;
        }

        public void setEnvironments(List<String> environments) {
            this.environments = environments;
        }

        public Boolean getIncludeDefaultEnvironment() {
            return includeDefaultEnvironment;
        }

        public void setIncludeDefaultEnvironment(Boolean includeDefaultEnvironment) {
            this.includeDefaultEnvironment = includeDefaultEnvironment;
        }

        public List<String> getClasspathElements() {
            return classpathElements;
        }

        public void setClasspathElements(List<String> classpathElements) {
            this.classpathElements = classpathElements;
        }

        public List<String> getAdditionalClasspathElements() {
            return additionalClasspathElements;
        }

        public void setAdditionalClasspathElements(List<String> additionalClasspathElements) {
            this.additionalClasspathElements = additionalClasspathElements;
        }

        public File getOutputDirectory() {
            return outputDirectory;
        }

        public void setOutputDirectory(File outputDirectory) {
            this.outputDirectory = outputDirectory;
        }
    }
}
