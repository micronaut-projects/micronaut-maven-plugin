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

import java.util.Locale;

/**
 * The values of {@code micronaut.docker.jdkAotCache.trainingMode}: how far the application goes in the JDK AOT cache
 * training run.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public enum TrainingMode {

    /**
     * The application loads its bean definitions and the classes they name, creates no bean and exits. It does not
     * start, so the training needs none of the services that its beans use.
     */
    LOAD,

    /**
     * The application starts, answers the training requests and is stopped.
     */
    START;

    /**
     * The plugin parameter that selects the mode.
     */
    static final String PARAMETER = "micronaut.docker.jdkAotCache.trainingMode";

    /**
     * @return the value of the mode in the Micronaut property {@value TrainingRunSwitch#MODE_PROPERTY}
     */
    String id() {
        return name().toLowerCase(Locale.ROOT);
    }

    /**
     * @return the plugin parameter set to this mode, for messages
     */
    String option() {
        return PARAMETER + "=" + name();
    }
}
