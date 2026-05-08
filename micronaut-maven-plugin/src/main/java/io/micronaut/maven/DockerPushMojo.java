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
package io.micronaut.maven;

import com.github.dockerjava.api.command.PushImageCmd;
import com.github.dockerjava.api.model.AuthConfig;
import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.File;
import java.util.Optional;

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

    private static final Logger LOG = LoggerFactory.getLogger(DockerPushMojo.class);

    @Inject
    public DockerPushMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                          ApplicationConfigurationService applicationConfigurationService, DockerService dockerService,
                          MavenSession mavenSession, MojoExecution mojoExecution) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession, mojoExecution);
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        Packaging packaging = Packaging.of(mavenProject.getPackaging());
        if (packaging == Packaging.DOCKER || packaging == Packaging.DOCKER_NATIVE || packaging == Packaging.DOCKER_CRAC) {
            if (packaging == Packaging.DOCKER && !shouldBuildWithDockerfile(new File(mavenProject.getBasedir(), DockerfileMojo.DOCKERFILE))) {
                if (validateJibDeployBuildGoal()) {
                    return;
                }
            }
            var images = getTags();

            // getTags() will automatically generate an image name if none is specified
            // To maintain error compatibility, check that an image name has been
            // manually specified.
            if (getConfiguredToImage().isPresent()) {
                for (String taggedImage : images) {
                    getLog().info("Pushing image: " + taggedImage);
                    try (PushImageCmd pushImageCmd = dockerService.pushImageCmd(taggedImage)) {
                        Optional<Credential> toCredentials = jibConfigurationService.getToCredentials();
                        Optional<Credential> credential = toCredentials.or(() -> jibConfigurationService.resolveCredentialForImage(taggedImage, LOG));
                        credential.ifPresent(cred -> {
                            var username = cred.getUsername();
                            var password = cred.getPassword();
                            AuthConfig authConfig = dockerService.getAuthConfigFor(taggedImage, username, password);
                            pushImageCmd.withAuthConfig(authConfig);
                        });

                        pushImageCmd.start().awaitCompletion();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    } catch (Exception e) {
                        throw new MojoExecutionException(e.getMessage(), e);
                    }
                }
            } else {
                throw new MojoFailureException("The plugin " + MavenProjectProperties.PLUGIN_KEY + " is misconfigured. Missing <to> tag");
            }
        } else {
            throw new MojoFailureException("The <packaging> must be set to either [" + Packaging.DOCKER.id() + "] or [" + Packaging.DOCKER_NATIVE.id() + "]");
        }
    }

    private boolean validateJibDeployBuildGoal() throws MojoExecutionException, MojoFailureException {
        validateJibBuildGoal();
        if (JIB_BUILD_GOAL_BUILD.equals(jibBuildGoal)) {
            String toImage = requireConfiguredToImageForJibRegistryBuild();
            getLog().info("Skipping Docker daemon push for " + toImage + " because jib.buildGoal=build publishes the image to the registry during the package phase.");
            return true;
        }
        if (JIB_BUILD_GOAL_BUILD_TAR.equals(jibBuildGoal)) {
            throw new MojoFailureException("jib.buildGoal=buildTar is not supported for deploy because it creates target/jib-image.tar instead of publishing to a registry. "
                + "Use jib.buildGoal=build with jib.to.image for daemonless registry deploy, or omit jib.buildGoal/use dockerBuild for the Docker daemon push path.");
        }
        return false;
    }

}
