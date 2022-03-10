package io.micronaut.build;

import com.github.dockerjava.api.command.PushImageCmd;
import com.github.dockerjava.api.model.AuthConfig;
import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.utility.RegistryAuthLocator;

import javax.inject.Inject;
import java.util.Optional;
import java.util.Set;

import static io.micronaut.build.DockerMojo.DOCKER_PACKAGING;
import static io.micronaut.build.DockerNativeMojo.DOCKER_NATIVE_PACKAGING;

/**
 * <p>Implementation of the <code>deploy</code> lifecycle for pushing Docker images</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, Execute the <code>deploy</code>
 * phase specifying the packaging type, eg:</p>
 *
 * <pre>mvn deploy -Dpackaging=docker-native</pre>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = "docker-push")
public class DockerPushMojo extends AbstractDockerMojo {

    @Inject
    public DockerPushMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                             ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        String packaging = mavenProject.getPackaging();
        if (DOCKER_PACKAGING.equals(packaging) || DOCKER_NATIVE_PACKAGING.equals(packaging)) {
            Set<String> images = getTags();

            // getTags() will automatically generate an image name if none is specified
            // To maintain error compatibility, check that an image name has been
            // manually specified.
            if (jibConfigurationService.getToImage().isPresent()) {
                for (String taggedImage : images) {
                    getLog().info("Pushing image: " + taggedImage);
                    try (PushImageCmd pushImageCmd = dockerService.pushImageCmd(taggedImage)) {
                        AuthConfig defaultAuthConfig = new AuthConfig();
                        if (jibConfigurationService.getCredentials().isPresent()) {
                            Credential credential = jibConfigurationService.getCredentials().get();
                            defaultAuthConfig
                                    .withUsername(credential.getUsername())
                                    .withPassword(credential.getPassword());
                        }
                        RegistryAuthLocator registryAuthLocator = RegistryAuthLocator.instance();
                        DockerImageName dockerImageName = DockerImageName.parse(taggedImage);
                        AuthConfig authConfig = registryAuthLocator.lookupAuthConfig(dockerImageName, defaultAuthConfig);

                        pushImageCmd
                                .withAuthConfig(authConfig)
                                .start()
                                .awaitCompletion();
                    } catch (Exception e) {
                        throw new MojoExecutionException(e.getMessage(), e);
                    }
                }
            } else {
                throw new MojoFailureException("The plugin " + MavenProjectProperties.PLUGIN_KEY + " is misconfigured. Missing <to> tag");
            }
        } else {
            throw new MojoFailureException("The <packaging> must be set to either [" + DOCKER_PACKAGING + "] or [" + DOCKER_NATIVE_PACKAGING + "]");
        }
    }
}
