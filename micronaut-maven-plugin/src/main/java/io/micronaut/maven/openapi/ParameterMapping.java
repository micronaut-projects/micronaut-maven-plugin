/*
 * Copyright 2017-2021 original authors
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
package io.micronaut.maven.openapi;

/**
 * A class used to specify parameter mapping.
 * Parameter mapping would map a given parameter to a specific type and name.
 */
public final class ParameterMapping {
    /**
     * The name of the parameter as described by the name field in specification.
     */
    private String name;

    /**
     * The location of parameter. Path parameters cannot be mapped, as this behavior should not be used.
     */
    private ParameterLocation location;

    /**
     * The type to which the parameter should be mapped. If multiple parameters have the same mapping,
     * only one parameter will be present. If set to null, the original parameter will be deleted.
     */
    private String mappedType;

    /**
     * The unique name of the parameter to be used as method parameter name.
     */
    private String mappedName;

    /**
     * Whether the mapped parameter requires validation.
     */
    private boolean isValidated;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ParameterLocation getLocation() {
        return location;
    }

    public void setLocation(ParameterLocation location) {
        this.location = location;
    }

    public String getMappedType() {
        return mappedType;
    }

    public void setMappedType(String mappedType) {
        this.mappedType = mappedType;
    }

    public String getMappedName() {
        return mappedName;
    }

    public void setMappedName(String mappedName) {
        this.mappedName = mappedName;
    }

    public boolean isValidated() {
        return isValidated;
    }

    public void setValidated(boolean validated) {
        isValidated = validated;
    }

    /**
     * The location of the parameter to be mapped.
     */
    public enum ParameterLocation {
        HEADER,
        QUERY,
        FORM,
        COOKIE,
        BODY
    }
}
