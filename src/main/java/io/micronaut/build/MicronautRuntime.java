package io.micronaut.build;

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

    public static final String PROPERTY = "micronaut.runtime";

}
