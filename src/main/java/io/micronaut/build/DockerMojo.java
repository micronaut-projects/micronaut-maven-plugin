package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import io.micronaut.build.jib.JibMicronautExtension;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.utils.io.FileUtils;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;

/**
 * <p>Allows using a provided Dockerfile.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 * using the <code>packaging</code> property, eg:</p>
 *
 * <pre>mvn package -Dpackaging=docker</pre>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Mojo(name = DockerMojo.DOCKER_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerMojo extends AbstractDockerMojo {

    public static final String DOCKER_PACKAGING = "docker";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService,
                      ApplicationConfigurationService applicationConfigurationService, DockerService dockerService) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
    }

    @Override
    public void execute() throws MojoExecutionException {
        File dockerfile = new File(mavenProject.getBasedir(), DockerfileMojo.DOCKERFILE);
        if (dockerfile.exists()) {
            try {
                getLog().info("Using provided Dockerfile: " + dockerfile.getAbsolutePath());
                mavenProject.getProperties().put(PropertyNames.SKIP, "true");

                copyDependencies();

                String targetDir = mavenProject.getBuild().getDirectory();
                File targetDockerfile = new File(targetDir, dockerfile.getName());
                FileUtils.copyFile(dockerfile, targetDockerfile);

                BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                        .withDockerfile(targetDockerfile)
                        .withTags(getTags())
                        .withBaseDirectory(new File(targetDir));
                dockerService.buildImage(buildImageCmd);
            } catch (IOException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            }
        } else {
            mavenProject.getProperties().setProperty(PropertyNames.FROM_IMAGE, JibMicronautExtension.DEFAULT_BASE_IMAGE);
        }
    }

}
