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
public final class ResponseBodyMapping {

    /**
     * The response header name that triggers the change of response type.
     */
    private String headerName;

    /**
     * The type in which will be used as the response type. The type must take
     * a single type parameter, which will be the original body.
     */
    private String mappedBodyType;

    /**
     * Whether the mapped body type needs to be supplied list items as property.
     */
    private boolean isListWrapper;

    /**
     * Whether the mapped response body type required validation.
     */
    private boolean isValidated;

    public String getHeaderName() {
        return headerName;
    }

    public void setHeaderName(String headerName) {
        this.headerName = headerName;
    }

    public String getMappedBodyType() {
        return mappedBodyType;
    }

    public void setMappedBodyType(String mappedBodyType) {
        this.mappedBodyType = mappedBodyType;
    }

    public boolean isListWrapper() {
        return isListWrapper;
    }

    public void setListWrapper(boolean listWrapper) {
        isListWrapper = listWrapper;
    }

    public boolean isValidated() {
        return isValidated;
    }

    public void setValidated(boolean validated) {
        isValidated = validated;
    }

}
