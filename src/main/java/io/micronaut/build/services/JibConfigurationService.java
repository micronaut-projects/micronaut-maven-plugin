package io.micronaut.build.services;

import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Exposes the Jib plugin configuration so that it can be read by other mojos.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class JibConfigurationService {

    private final MavenProject mavenProject;

    private Xpp3Dom configuration;
    private Xpp3Dom to;
    private Xpp3Dom from;

    @Inject
    public JibConfigurationService(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
        final Plugin plugin = mavenProject.getPlugin(MavenProjectProperties.PLUGIN_KEY);
        if (plugin != null && plugin.getConfiguration() != null) {
            configuration = (Xpp3Dom) plugin.getConfiguration();
            to = configuration.getChild("to");
            from = configuration.getChild("from");
        }
    }

    public Optional<String> getToImage() {
        if (to != null) {
            return Optional.ofNullable(to.getChild("image").getValue());
        }
        return Optional.empty();
    }

    public Optional<String> getFromImage() {
        if (from != null) {
            return Optional.ofNullable(from.getChild("image").getValue());
        }
        return Optional.empty();
    }

    public Set<String> getTags() {
        if (to != null) {
            Xpp3Dom tags = to.getChild("tags");
            if (tags != null && tags.getChildCount() > 0) {
                return Arrays.stream(tags.getChildren())
                        .map(Xpp3Dom::getValue)
                        .collect(Collectors.toSet());
            }
        }
        return Collections.emptySet();
    }
}
