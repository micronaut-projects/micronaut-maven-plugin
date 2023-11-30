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
package io.micronaut.maven.testresources;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Base configuration class for Test Resources.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 3.5.1
 */
@JsonRootName("configuration")
@JsonIgnoreProperties(ignoreUnknown = true)
public class TestResourcesConfiguration {

    public static final String DISABLED = "false";
    public static final String CONFIG_PROPERTY_PREFIX = "micronaut.test.resources.";

    private static final String PROPERTY_ENABLED = "enabled";

    /**
     * Whether to enable or disable Micronaut test resources support.
     */
    @Parameter(property =  CONFIG_PROPERTY_PREFIX + PROPERTY_ENABLED, defaultValue = DISABLED)
    @JsonProperty(PROPERTY_ENABLED)
    protected boolean testResourcesEnabled;

    /**
     * Whether the test resources service should be shared between independent builds
     * (e.g different projects, even built with different build tools).
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "shared", defaultValue = DISABLED)
    protected boolean shared;

    /**
     * Allows configuring a namespace for the shared test resources server. This can be used in case it makes sense to
     * have different instances of shared services, for example when independent builds sets share different services.
     *
     * @since 3.5.1
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "namespace")
    protected String sharedServerNamespace;

    /**
     * Allows starting the test resources server in debug mode. The server will be started with the ability
     * to attach a remote debugger on port 8000.
     *
     * @since 4.1.1
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "debug-server", defaultValue = DISABLED)
    protected boolean debugServer;

    /**
     * @return Whether to enable or disable Micronaut test resources support.
     */
    public boolean isTestResourcesEnabled() {
        return testResourcesEnabled;
    }

    /**
     * @return Whether the test resources service should be shared between independent builds.
     */
    public boolean isShared() {
        return shared;
    }

    /**
     * @return The shared server namespace (if any).
     */
    public String getSharedServerNamespace() {
        return sharedServerNamespace;
    }
}
