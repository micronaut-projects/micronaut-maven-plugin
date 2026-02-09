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

import java.util.Locale;

/**
 * Output format for configuration validation reports.
 */
enum ConfigurationValidationFormat {
    /**
     * Generate JSON report ({@code configuration-errors.json}).
     */
    JSON,

    /**
     * Generate HTML report ({@code configuration-errors.html}).
     */
    HTML,

    /**
     * Generate both JSON and HTML reports.
     */
    BOTH;

    /**
     * Parse a format value.
     *
     * @param value The configured format, or {@code null}
     * @return The parsed format (defaults to {@link #BOTH})
     */
    static ConfigurationValidationFormat parse(String value) {
        if (value == null || value.isBlank()) {
            return BOTH;
        }
        String normalized = value.trim().toLowerCase(Locale.ENGLISH);
        return switch (normalized) {
            case "json" -> JSON;
            case "html" -> HTML;
            case "both" -> BOTH;
            default -> throw new IllegalArgumentException("Invalid format: " + value);
        };
    }
}
