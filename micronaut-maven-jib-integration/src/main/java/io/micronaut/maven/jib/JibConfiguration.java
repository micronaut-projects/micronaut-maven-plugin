/*
 * Copyright 2017-2024 original authors
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
package io.micronaut.maven.jib;

import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Represents the Jib plugin configuration.
 *
 * @param from The from configuration.
 * @param to The to configuration.
 * @param container The container configuration.
 * @since 4.7.0
 */
public record JibConfiguration(Optional<FromConfiguration> from, Optional<ToConfiguration> to, Optional<ContainerConfiguration> container) {

    /**
     * Represents the authentication configuration.
     *
     * @param username The username.
     * @param password The password.
     */
    public record AuthConfiguration(Optional<String> username, Optional<String> password) { }

    /**
     * Represents the from configuration.
     *
     * @param image The image.
     * @param auth The authentication configuration.
     */
    public record FromConfiguration(Optional<String> image, Optional<AuthConfiguration> auth) { }

    /**
     * Represents the to configuration.
     *
     * @param image The image.
     * @param tags The tags.
     * @param auth The authentication configuration.
     */
    public record ToConfiguration(Optional<String> image, Set<String> tags, Optional<AuthConfiguration> auth) { }

    /**
     * Represents the container configuration.
     *
     * @param workingDirectory The working directory.
     * @param ports The ports.
     * @param args The arguments.
     * @param labels The labels.
     */
    public record ContainerConfiguration(Optional<String> workingDirectory, Set<String> ports, List<String> args, Set<String> labels) { }
}
