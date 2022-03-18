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
    DOCKER_NATIVE("docker-native");

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
