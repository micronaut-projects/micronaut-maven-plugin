package io.micronaut.build;

import com.google.cloud.tools.jib.api.*;
import com.google.cloud.tools.jib.frontend.CredentialRetrieverFactory;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.util.Optional;

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
            Optional<String> toImage = jibConfigurationService.getToImage();
            if (toImage.isPresent()) {
                getLog().info("Pushing image: " + toImage.get());
                try {
                    ImageReference imageReference = ImageReference.parse(toImage.get());
                    DockerDaemonImage dockerDaemonImage = DockerDaemonImage.named(toImage.get());
                    RegistryImage targetImage = RegistryImage.named(imageReference);

                    CredentialRetrieverFactory credentialRetrieverFactory = CredentialRetrieverFactory
                            .forImage(imageReference, logEvent -> getLog().info(logEvent.getMessage()));
                    targetImage.addCredentialRetriever(credentialRetrieverFactory.dockerConfig());
                    targetImage.addCredentialRetriever(credentialRetrieverFactory.wellKnownCredentialHelpers());

                    jibConfigurationService.getCredentials().ifPresent(c -> targetImage.addCredential(c.getUsername(), c.getPassword()));
                    jibConfigurationService.getCredHelper().ifPresent(credHelper -> targetImage.addCredentialRetriever(credentialRetrieverFactory.dockerCredentialHelper(credHelper)));

                    Containerizer containerizer = Containerizer.to(targetImage);
                    Jib.from(dockerDaemonImage).containerize(containerizer);
                } catch (Exception e) {
                    throw new MojoExecutionException(e.getMessage(), e);
                }
            } else {
                throw new MojoFailureException("The plugin " + MavenProjectProperties.PLUGIN_KEY + " is misconfigured. Missing <to> tag");
            }
        } else {
            throw new MojoFailureException("The <packaging> must be set to either [" + DOCKER_PACKAGING + "] or [" + DOCKER_NATIVE_PACKAGING + "]");
        }
    }
}
