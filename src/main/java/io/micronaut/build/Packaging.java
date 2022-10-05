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
package io.micronaut.build;

/**
 * Packaging types supported by this plugin.
 *
 * @author Álvaro Sánchez-Mariscal
 */
public enum Packaging {
    JAR("jar"),
    NATIVE_IMAGE("native-image"),
    DOCKER("docker"),
    DOCKER_NATIVE("docker-native"),
    DOCKER_CRAC("docker-crac");

    private final String id;

    Packaging(String id) {
        this.id = id;
    }

    public String id() {
        return this.id;
    }

    public static Packaging of(String value) {
        return Packaging.valueOf(value.replace("-", "_").toUpperCase());
    }
}
