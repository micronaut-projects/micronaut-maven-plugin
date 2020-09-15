package io.micronaut.build;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * The packaging kind of the application.
 *
 * @author graemerocher
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
    NETTY("io.micronaut:micronaut-http-server-netty"),
    /**
     * Tomcat server.
     */
    TOMCAT("io.micronaut.servlet:micronaut-http-server-tomcat"),
    /**
     * Jetty server.
     */
    JETTY("io.micronaut.servlet:micronaut-http-server-jetty"),
    /**
     * Undertow server.
     */
    UNDERTOW("io.micronaut.servlet:micronaut-http-server-undertow"),
    /**
     * AWS lambda packaged as a Jar file.
     */
    LAMBDA(),
    /**
     * Oracle Cloud Function, packaged as a docker container.
     */
    ORACLE_FUNCTION(),
    /**
     * Google Cloud Function, packaged as a Fat JAR.
     */
    GOOGLE_FUNCTION(),
    /**
     * Azure Cloud Function.
     */
    AZURE_FUNCTION();

    private final Map<String, List<String>> dependencies;

    MicronautRuntime(String... dependencies) {
        this.dependencies = Collections.emptyMap();
    }

    MicronautRuntime(Map<String, List<String>> dependencies) {
        this.dependencies = dependencies;
    }

    public Map<String, List<String>> getDependencies() {
        return dependencies;
    }
}
