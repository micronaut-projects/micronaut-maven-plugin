/*
 * Copyright 2017-2022 original authors
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
package io.micronaut.maven.aot.internal;

import java.util.Optional;

/**
 * Packaging types supported by Micronaut's integrated AOT execution.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.0.0
 */
public enum AotPackaging {
    JAR("jar"),
    NATIVE_IMAGE("native-image"),
    DOCKER("docker"),
    DOCKER_NATIVE("docker-native"),
    DOCKER_CRAC("docker-crac"),
    K8S("k8s"),
    OPENSHIFT("openshift");

    private final String id;

    AotPackaging(String id) {
        this.id = id;
    }

    public static AotPackaging of(String value) {
        return AotPackaging.valueOf(value.replace("-", "_").toUpperCase());
    }

    public static Optional<AotPackaging> find(String value) {
        try {
            return Optional.of(of(value));
        } catch (IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    public String id() {
        return id;
    }
}
