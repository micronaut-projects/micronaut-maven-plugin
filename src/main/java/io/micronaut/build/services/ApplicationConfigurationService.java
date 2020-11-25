package io.micronaut.build.services;

import io.micronaut.context.env.*;
import io.micronaut.context.env.yaml.YamlPropertySourceLoader;
import io.micronaut.core.io.file.DefaultFileSystemResourceLoader;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Parses the Micronaut application configuration.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class ApplicationConfigurationService {

    public static final String DEFAULT_PORT = "8080";

    private final MavenProject mavenProject;
    private final Map<String, Object> applicationConfiguration;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public ApplicationConfigurationService(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
        this.applicationConfiguration = parseApplicationConfiguration();
    }

    public String getServerPort() {
        return applicationConfiguration.getOrDefault("MICRONAUT_SERVER_PORT", applicationConfiguration.getOrDefault("micronaut.server.port", DEFAULT_PORT)).toString();
    }

    private Map<String, Object> parseApplicationConfiguration() {
        Map<String, Object> configuration = new HashMap<>();

        PropertySourceLoader[] loaders = {
                new YamlPropertySourceLoader(),
                new PropertiesPropertySourceLoader()
        };

        for (PropertySourceLoader loader : loaders) {
            Optional<PropertySource> propertySource = loader.load("application", new DefaultFileSystemResourceLoader(new File(mavenProject.getBasedir(), "src/main/resources")));
            if (propertySource.isPresent()) {
                MapPropertySource mapPropertySource = (MapPropertySource) propertySource.get();
                configuration.putAll(mapPropertySource.asMap());
            }
        }

        MapPropertySource[] propertySources = {
                new EnvironmentPropertySource(),
                new SystemPropertiesPropertySource()
        };
        for (MapPropertySource propertySource : propertySources) {
            configuration.putAll(propertySource.asMap());
        }

        return configuration;
    }
}
