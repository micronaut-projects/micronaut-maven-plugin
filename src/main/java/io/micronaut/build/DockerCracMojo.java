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
package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import io.micronaut.build.services.ApplicationConfigurationService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.JibConfigurationService;
import io.micronaut.core.annotation.Experimental;
import org.apache.commons.io.IOUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * <p>Implementation of the <code>docker-crac</code> packaging.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 * using the <code>packaging</code> property, eg:</p>
 *
 * <pre>mvn package -Dpackaging=docker-crac</pre>
 *
 * This is a two stage process. First a docker image is built that runs the application under a CRaC enabled JDK. Then
 * the application is warmed up via a shell script. And then a checkpoint is taken via a signal using jcmd.
 *
 * The second stage takes this checkpoint, and creates the final image containing it plus a run script which passes the
 * correct flags to the CRaC enabled JDK.
 *
 * @author Tim Yates
 * @since 3.5.0
 */
@Experimental
@Mojo(name = DockerCracMojo.DOCKER_CRAC_PACKAGING, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class DockerCracMojo extends AbstractDockerMojo {

    public static final String DOCKER_CRAC_PACKAGING = "docker-crac";
    public static final String CHECKPOINT_SCRIPT_NAME = "checkpoint.sh";
    public static final String WARMUP_SCRIPT_NAME = "warmup.sh";
    public static final String RUN_SCRIPT_NAME = "run.sh";
    public static final String DEFAULT_READINESS_COMMAND = "curl --output /dev/null --silent --head http://localhost:8080";
    public static final String CRAC_READINESS_PROPERTY = "crac.readiness";
    public static final String DEFAULT_BASE_IMAGE = "ubuntu:20.04";

    private static final EnumSet<PosixFilePermission> POSIX_FILE_PERMISSIONS = EnumSet.of(
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_EXECUTE,
            PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE, PosixFilePermission.GROUP_EXECUTE,
            PosixFilePermission.OTHERS_READ, PosixFilePermission.OTHERS_EXECUTE
    );

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerCracMojo(
            MavenProject mavenProject,
            JibConfigurationService jibConfigurationService,
            ApplicationConfigurationService applicationConfigurationService,
            DockerService dockerService
    ) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService);
    }

    @Override
    public void execute() throws MojoExecutionException {
        try {
            copyDependencies();

            MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());

            switch (runtime.getBuildStrategy()) {
                case LAMBDA:
                    throw new MojoExecutionException("Lambda Functions are currently unsupported");

                case ORACLE_FUNCTION:
                    throw new MojoExecutionException("Oracle Functions are currently unsupported");

                case DEFAULT:
                default:
                    buildDockerCrac();
                    break;
            }
        } catch (InvalidImageReferenceException iire) {
            String message = "Invalid image reference "
                    + iire.getInvalidReference()
                    + ", perhaps you should check that the reference is formatted correctly according to " +
                    "https://docs.docker.com/engine/reference/commandline/tag/#extended-description" +
                    "\nFor example, slash-separated name components cannot have uppercase letters";
            throw new MojoExecutionException(message);
        } catch (IOException | IllegalArgumentException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void buildDockerCrac() throws IOException, InvalidImageReferenceException {
        String checkpointImage = buildCheckpointDockerfile();
        getLog().info("CRaC Checkpoint image: " + checkpointImage);
        dockerService.runPrivilegedImageAndWait(checkpointImage, new File(mavenProject.getBuild().getDirectory(), "cr").getAbsolutePath() + ":/home/app/cr");
        buildFinalDockerfile(checkpointImage);
    }

    private String buildCheckpointDockerfile() throws IOException {
        String name = mavenProject.getArtifactId() + "-crac-checkpoint";
        Set<String> checkpointTags = Collections.singleton(name);
        copyScripts(CHECKPOINT_SCRIPT_NAME, WARMUP_SCRIPT_NAME, RUN_SCRIPT_NAME);
        File dockerfile = dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_CRAC_CHECKPOINT);
        BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                .withDockerfile(dockerfile)
                .withBuildArg("BASE_IMAGE", getFromImage().orElse(DEFAULT_BASE_IMAGE))
                .withTags(checkpointTags);
        dockerService.buildImage(buildImageCmd);
        return name;
    }

    private String getReadinessCommand() {
        String property = mavenProject.getProperties().getProperty(DockerCracMojo.CRAC_READINESS_PROPERTY);
        return property == null ? DockerCracMojo.DEFAULT_READINESS_COMMAND : property;
    }

    private void buildFinalDockerfile(String checkpointContainerId) throws IOException, InvalidImageReferenceException {
        Set<String> tags = getTags();
        for (String tag : tags) {
            ImageReference.parse(tag);
        }
        copyScripts(RUN_SCRIPT_NAME);
        File dockerfile = dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_CRAC);
        BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
                .withDockerfile(dockerfile)
                .withBuildArg("BASE_IMAGE", getFromImage().orElse(DEFAULT_BASE_IMAGE))
                .withBuildArg("CHECKPOINT_IMAGE", checkpointContainerId)
                .withTags(getTags());
        dockerService.buildImage(buildImageCmd);
    }

    private void copyScripts(String... scriptNames) throws IOException {
        List<String> imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        String readinessCommand = getReadinessCommand();
        mavenProject.setArtifactFilter(artifact -> imageClasspathScopes.contains(artifact.getScope()));
        File target = new File(mavenProject.getBuild().getDirectory(), "scripts");
        if (!target.exists()) {
            target.mkdirs();
        }
        processScripts(target, s -> s.replace("@READINESS@", readinessCommand).replace("@MAINCLASS@", mainClass), scriptNames);
    }

    private void processScripts(File target, UnaryOperator<String> replacement, String... scriptNames) throws IOException {
        for (String script : scriptNames) {
            File localOverride = new File(mavenProject.getBasedir(), script);
            try (InputStream resourceAsStream = localOverride.exists() ?
                    Files.newInputStream(localOverride.toPath()) :
                    DockerCracMojo.class.getResourceAsStream("/cracScripts/" + script)) {
                if (resourceAsStream == null) {
                    throw new IOException("Could not find script " + script);
                }
                List<String> tokenized = replaceTokensInTextStream(resourceAsStream, replacement);
                Path outputPath = target.toPath().resolve(script);
                writeStringsToFile(tokenized, outputPath);
                Files.setPosixFilePermissions(outputPath, POSIX_FILE_PERMISSIONS);
            }
        }
    }

    private static List<String> replaceTokensInTextStream(InputStream resourceAsStream, UnaryOperator<String> replacement) throws IOException {
        return IOUtils.readLines(resourceAsStream, StandardCharsets.UTF_8)
                .stream()
                .map(replacement)
                .collect(Collectors.toList());
    }

    private static void writeStringsToFile(List<String> tokenized, Path outputPath) throws IOException {
        try (BufferedWriter out = Files.newBufferedWriter(outputPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
            IOUtils.writeLines(tokenized, System.getProperty("line.separator"), out);
        }
    }
}
