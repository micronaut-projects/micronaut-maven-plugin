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
package io.micronaut.maven.jib;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static io.micronaut.maven.jib.JibConfiguration.*;

/**
 * Exposes the Jib plugin configuration so that it can be read by other mojos.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class JibConfigurationService {

    private final Optional<JibConfiguration> configuration;

    @Inject
    public JibConfigurationService(MavenProject mavenProject) {
        final Plugin plugin = mavenProject.getPlugin(MavenProjectProperties.PLUGIN_KEY);
        if (plugin != null && plugin.getConfiguration() != null) {
            final XmlMapper mapper = XmlMapper.builder()
                    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
                    .findAndAddModules()
                    .build();
            try {
                configuration = Optional.ofNullable(mapper.readValue(plugin.getConfiguration().toString(), JibConfiguration.class));
            } catch (JsonProcessingException e) {
                throw new IllegalArgumentException("Error parsing Jib plugin configuration", e);
            }
        } else {
            configuration = Optional.empty();
        }
    }

    /**
     * @return the <code>to.image</code> configuration.
     */
    public Optional<String> getToImage() {
        final String value = configuration.flatMap(c -> c.to().flatMap(ToConfiguration::image)).orElse(null);
        return Optional.ofNullable(System.getProperties().getProperty(PropertyNames.TO_IMAGE, value));
    }

    /**
     * @return the <code>from.image</code> configuration.
     */
    public Optional<String> getFromImage() {
        final String value = configuration.flatMap(c -> c.from().flatMap(FromConfiguration::image)).orElse(null);
        return Optional.ofNullable(System.getProperties().getProperty(PropertyNames.FROM_IMAGE, value));
    }

    /**
     * @return the <code>to.tags</code> configuration.
     */
    public Set<String> getTags() {
        final Set<String> tags = configuration.flatMap(c -> c.to().map(ToConfiguration::tags)).orElse(Collections.emptySet());
        return Optional.ofNullable(System.getProperties().getProperty(PropertyNames.TO_TAGS))
                .map(JibConfigurationService::parseCommaSeparatedList)
                .orElse(tags);
    }

    /**
     * @return the <code>to.auth.username</code> and <code>to.auth.password</code> configuration.
     */
    public Optional<Credential> getToCredentials() {
        String usernameProperty = System.getProperties().getProperty(PropertyNames.TO_AUTH_USERNAME);
        String passwordProperty = System.getProperties().getProperty(PropertyNames.TO_AUTH_PASSWORD);
        if (usernameProperty != null || passwordProperty != null) {
            return Optional.of(Credential.from(usernameProperty, passwordProperty));
        } else {
            return configuration
                    .flatMap(c -> c.to().flatMap(ToConfiguration::auth))
                    .map(this::getCredentials);
        }
    }

    /**
     * @return the <code>from.auth.username</code> and <code>from.auth.password</code> configuration.
     */
    public Optional<Credential> getFromCredentials() {
        String usernameProperty = System.getProperties().getProperty(PropertyNames.FROM_AUTH_USERNAME);
        String passwordProperty = System.getProperties().getProperty(PropertyNames.FROM_AUTH_PASSWORD);
        if (usernameProperty != null || passwordProperty != null) {
            return Optional.of(Credential.from(usernameProperty, passwordProperty));
        } else {
            return configuration
                    .flatMap(c -> c.from().flatMap(FromConfiguration::auth))
                    .map(this::getCredentials);
        }

    }

    private Credential getCredentials(AuthConfiguration authConfiguration) {
        return Credential.from(
                authConfiguration.username().orElse(null),
                authConfiguration.password().orElse(null)
        );
    }

    /**
     * @return the <code>container.workingDirectory</code> configuration.
     */
    public Optional<String> getWorkingDirectory() {
        final String value = configuration.flatMap(c -> c.container().flatMap(ContainerConfiguration::workingDirectory)).orElse(null);
        return Optional.ofNullable(System.getProperties().getProperty(PropertyNames.CONTAINER_WORKING_DIRECTORY, value));
    }

    /**
     * @return the <code>container.args</code> configuration.
     */
    public List<String> getArgs() {
        final List<String> args = configuration.flatMap(c -> c.container().map(ContainerConfiguration::args)).orElse(Collections.emptyList());
        return Optional.ofNullable(System.getProperties().getProperty(PropertyNames.CONTAINER_ARGS))
                .map(JibConfigurationService::parseCommaSeparatedList)
                .map(List::copyOf)
                .orElse(args);
    }

    private static Set<String> parseCommaSeparatedList(String list) {
        String[] parts = list.split(",");
        var items = new HashSet<String>(parts.length);
        for (String part : parts) {
            items.add(part.trim());
        }
        return items;
    }
}
