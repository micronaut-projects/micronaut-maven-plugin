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
package io.micronaut.maven.jdkaotcache;

import io.micronaut.core.annotation.Internal;
import org.apache.maven.plugin.MojoExecutionException;

import java.util.Locale;
import java.util.Optional;

/**
 * How far the application goes in the JDK AOT cache training run. The names are the values of
 * {@code micronaut.docker.jdkAotCache.trainingMode}, and the values that Micronaut core reads from
 * {@value TrainingRunSwitch#MODE_PROPERTY}.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public enum TrainingMode {

    /**
     * The application loads its bean definitions and the classes they name, creates no bean and exits. It does not
     * start, so the training needs none of the services the application uses.
     */
    LOAD,

    /**
     * The application starts, answers the warm-up requests and is stopped.
     */
    START;

    /**
     * The plugin parameter that selects the mode.
     */
    public static final String OPTION = "micronaut.docker.jdkAotCache.trainingMode";

    /**
     * @return the value of the mode, in the plugin parameter and in the Micronaut property
     */
    public String id() {
        return name().toLowerCase(Locale.ROOT);
    }

    /**
     * @param configured the configured value of {@value #OPTION}, which may be {@code null} or blank
     * @return the mode, in any case, or empty if none is configured
     * @throws MojoExecutionException if the value is not a mode
     */
    public static Optional<TrainingMode> parse(String configured) throws MojoExecutionException {
        if (configured == null || configured.isBlank()) {
            return Optional.empty();
        }
        String value = configured.trim();
        for (TrainingMode mode : values()) {
            if (mode.id().equalsIgnoreCase(value)) {
                return Optional.of(mode);
            }
        }
        throw new MojoExecutionException("Invalid " + OPTION + " '" + value + "': it must be " + LOAD.id() + " or "
            + START.id());
    }
}
