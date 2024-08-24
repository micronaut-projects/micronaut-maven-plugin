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

import com.github.dockerjava.api.command.BuildImageCmd;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import io.micronaut.maven.core.DockerBuildStrategy;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.StandardCopyOption;

import static io.micronaut.maven.DockerfileMojo.DOCKERFILE_ORACLE_CLOUD;

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
                      ApplicationConfigurationService applicationConfigurationService, DockerService dockerService,
                      MavenSession mavenSession, MojoExecution mojoExecution) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession,
            mojoExecution);
    }

    @Override
    public void execute() throws MojoExecutionException {
        var providedDockerfile = new File(mavenProject.getBasedir(), DockerfileMojo.DOCKERFILE);
        if (shouldBuildWithDockerfile(providedDockerfile)) {
            var dockerfile = determineDockerfile(providedDockerfile);
            buildDockerfile(dockerfile);
        } else if (jibConfigurationService.getFromImage().isEmpty()) {
            mavenProject.getProperties().setProperty(PropertyNames.FROM_IMAGE, getBaseImage());
        }
    }

    private File determineDockerfile(File providedDockerfile) throws MojoExecutionException {
        if (providedDockerfile.exists()) {
            return providedDockerfile;
        }
        try {
            return dockerService.loadDockerfileAsResource(DOCKERFILE_ORACLE_CLOUD);
        } catch (IOException e) {
            throw new MojoExecutionException("Error loading Dockerfile", e);
        }
    }

    private boolean shouldBuildWithDockerfile(File providedDockerfile) {
        var runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());
        return providedDockerfile.exists() || runtime.getBuildStrategy() == DockerBuildStrategy.ORACLE_FUNCTION;
    }

    private void buildDockerfile(File dockerfile) throws MojoExecutionException {
        try {
            var runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());
            if (runtime.getBuildStrategy() == DockerBuildStrategy.ORACLE_FUNCTION) {
                oracleCloudFunctionCmd(dockerfile);
                DockerfileMojo.processOracleFunctionDockerfile(dockerfile);
            }
            getLog().info("Using Dockerfile: " + dockerfile.getAbsolutePath());
            mavenProject.getProperties().put(PropertyNames.SKIP, "true");

            copyDependencies();

            String targetDir = mavenProject.getBuild().getDirectory();
            var targetDockerfile = new File(targetDir, dockerfile.getName());
            Files.copy(dockerfile.toPath(), targetDockerfile.toPath(), LinkOption.NOFOLLOW_LINKS,
                StandardCopyOption.REPLACE_EXISTING);

            BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                .withDockerfile(targetDockerfile)
                .withTags(getTags())
                .withBaseDirectory(new File(targetDir));
            getNetworkMode().ifPresent(buildImageCmd::withNetworkMode);
            dockerService.buildImage(buildImageCmd);
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

}
