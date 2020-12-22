package io.micronaut.build.services;

import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Exposes the Jib plugin configuration so that it can be read by other mojos.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class JibConfigurationService {

    private static final String IMAGE = "image";
    private static final String CONTAINER = "container";
    private static final String WORKING_DIRECTORY = "workingDirectory";

    private Xpp3Dom configuration;
    private Xpp3Dom to;
    private Xpp3Dom from;

    @Inject
    public JibConfigurationService(MavenProject mavenProject) {
        final Plugin plugin = mavenProject.getPlugin(MavenProjectProperties.PLUGIN_KEY);
        if (plugin != null && plugin.getConfiguration() != null) {
            configuration = (Xpp3Dom) plugin.getConfiguration();
            to = configuration.getChild("to");
            from = configuration.getChild("from");
        }
    }

    public Optional<String> getToImage() {
        Optional<String> result;
        String propertyValue = System.getProperties().getProperty(PropertyNames.TO_IMAGE);
        if (propertyValue != null) {
            result = Optional.of(propertyValue);
        } else if (to != null) {
            result = Optional.ofNullable(to.getChild(IMAGE).getValue());
        } else {
            result = Optional.empty();
        }
        return result;
    }

    public Optional<String> getFromImage() {
        Optional<String> result;
        String propertyValue = System.getProperties().getProperty(PropertyNames.FROM_IMAGE);
        if (propertyValue != null) {
            result = Optional.of(propertyValue);
        } else if (from != null) {
            result = Optional.ofNullable(from.getChild(IMAGE).getValue());
        } else {
            result = Optional.empty();
        }
        return result;
    }

    public Set<String> getTags() {
        Set<String> result = null;
        String propertyValue = System.getProperties().getProperty(PropertyNames.TO_TAGS);
        if (propertyValue != null) {
            result = new HashSet<>(parseCommaSeparatedList(propertyValue));
        } else {
            if (to != null) {
                Xpp3Dom tags = to.getChild("tags");
                if (tags != null && tags.getChildCount() > 0) {
                    result = Arrays.stream(tags.getChildren())
                            .map(Xpp3Dom::getValue)
                            .collect(Collectors.toSet());
                }
            }
            if (result == null) {
                result = Collections.emptySet();
            }
        }
        return result;
    }

    public Optional<Credential> getCredentials() {
        Optional<Credential> result = Optional.empty();
        String usernameProp = System.getProperties().getProperty(PropertyNames.TO_AUTH_USERNAME);
        String passwordProp = System.getProperties().getProperty(PropertyNames.TO_AUTH_PASSWORD);
        if (usernameProp != null && passwordProp != null) {
            result = Optional.of(Credential.from(usernameProp, passwordProp));
        } else {
            if (to != null) {
                Xpp3Dom auth = to.getChild("auth");
                if (auth != null) {
                    Xpp3Dom username = auth.getChild("username");
                    Xpp3Dom password = auth.getChild("password");
                    if (username != null && password != null) {
                        result = Optional.of(Credential.from(username.getValue(), password.getValue()));
                    }
                }
            }
        }
        return result;
    }

    public Optional<String> getCredHelper() {
        Optional<String> result = Optional.empty();
        String propertyValue = System.getProperties().getProperty(PropertyNames.TO_CRED_HELPER);
        if (propertyValue != null) {
            result = Optional.of(propertyValue);
        } else {
            if (to != null) {
                Xpp3Dom credHelper = to.getChild("credHelper");
                if (credHelper != null) {
                    result = Optional.of(credHelper.getValue());
                }
            }
        }
        return result;
    }

    public Optional<String> getWorkingDirectory() {
        if (configuration != null) {
            Xpp3Dom container = configuration.getChild(CONTAINER);
            if (container != null && container.getChild(WORKING_DIRECTORY) != null) {
                return Optional.ofNullable(container.getChild(WORKING_DIRECTORY).getValue());
            }
        }
        return Optional.empty();
    }

    public List<String> getArgs() {
        List<String> result = new ArrayList<>();
        if (configuration != null) {
            Xpp3Dom container = configuration.getChild(CONTAINER);
            if (container != null) {
                Xpp3Dom args = container.getChild("args");
                if (args.getChildCount() > 0) {
                    for (Xpp3Dom arg : args.getChildren()) {
                        result.add(arg.getValue());
                    }
                } else {
                    result.add(args.getValue());
                }
            }
        }
        return result;
    }

    private static Set<String> parseCommaSeparatedList(String list) {
        String[] parts = list.split(",");
        Set<String> items = new HashSet<>(parts.length);
        for (String part : parts) {
            items.add(part.trim());
        }
        return items;
    }
}
