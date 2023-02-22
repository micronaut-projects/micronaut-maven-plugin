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
package io.micronaut.maven.core;

/**
 * The packaging kind of the application.
 *
 * @author graemerocher
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
public enum MicronautRuntime {

    /**
     * No specific runtime specified.
     */
    NONE(),

    /**
     * Default packaging.
     */
    NETTY(),

    /**
     * Tomcat server.
     */
    TOMCAT(),

    /**
     * Jetty server.
     */
    JETTY(),

    /**
     * Undertow server.
     */
    UNDERTOW(),

    /**
     * AWS lambda packaged as a Jar file.
     */
    LAMBDA(DockerBuildStrategy.LAMBDA),

    /**
     * Oracle Cloud Function, packaged as a docker container.
     */
    ORACLE_FUNCTION(DockerBuildStrategy.ORACLE_FUNCTION),

    /**
     * Google Cloud Function, packaged as a Fat JAR.
     */
    GOOGLE_FUNCTION(),

    /**
     * Azure Cloud Function.
     */
    AZURE_FUNCTION();

    public static final String PROPERTY = "micronaut.runtime";

    private final DockerBuildStrategy buildStrategy;

    MicronautRuntime() {
        this.buildStrategy = DockerBuildStrategy.DEFAULT;
    }

    MicronautRuntime(DockerBuildStrategy buildStrategy) {
        this.buildStrategy = buildStrategy;
    }

    /**
     * @return The docker build strategy
     */
    public DockerBuildStrategy getBuildStrategy() {
        return buildStrategy;
    }

}
