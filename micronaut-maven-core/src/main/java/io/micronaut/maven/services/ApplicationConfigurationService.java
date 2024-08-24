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
package io.micronaut.maven.services;

import io.micronaut.context.env.EnvironmentPropertySource;
import io.micronaut.context.env.MapPropertySource;
import io.micronaut.context.env.PropertiesPropertySourceLoader;
import io.micronaut.context.env.PropertySource;
import io.micronaut.context.env.PropertySourceLoader;
import io.micronaut.context.env.SystemPropertiesPropertySource;
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

    /**
     * Determines the application port by looking at the <code>MICRONAUT_SERVER_PORT</code> environment variable, the
     * <code>micronaut.server.port</code> configuration property, or falls back to a default port.
     *
     * @return The application port
     */
    public String getServerPort() {
        return applicationConfiguration.getOrDefault("MICRONAUT_SERVER_PORT", applicationConfiguration.getOrDefault("micronaut.server.port", DEFAULT_PORT)).toString();
    }

    private Map<String, Object> parseApplicationConfiguration() {
        var configuration = new HashMap<String, Object>();

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
