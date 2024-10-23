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
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.InvalidImageReferenceException;
import io.micronaut.core.annotation.Experimental;
import io.micronaut.maven.core.MicronautRuntime;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.commons.io.IOUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.filtering.MavenFilteringException;
import org.apache.maven.shared.filtering.MavenReaderFilter;
import org.apache.maven.shared.filtering.MavenReaderFilterRequest;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Properties;
import java.util.Set;

/**
 * <p>Implementation of the <code>docker-crac</code> packaging.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly. Instead, specify the packaging type
 * using the <code>packaging</code> property, eg:</p>
 *
 * <pre>mvn package -Dpackaging=docker-crac</pre>
 * <p>
 * This is a two stage process. First a docker image is built that runs the application under a CRaC enabled JDK. Then
 * the application is warmed up via a shell script. And then a checkpoint is taken via a signal using jcmd.
 * <p>
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
    public static final String DEFAULT_CRAC_CHECKPOINT_TIMEOUT = "60";
    public static final String CRAC_CHECKPOINT_NETWORK_PROPERTY = "crac.checkpoint.network";
    public static final String CRAC_CHECKPOINT_TIMEOUT_PROPERTY = "crac.checkpoint.timeout";

    public static final String CRAC_JAVA_VERSION = "crac.java.version";

    public static final String CRAC_ARCHITECTURE = "crac.arch";

    public static final String CRAC_OS = "crac.os";
    public static final String DEFAULT_CRAC_OS = "linux-glibc";

    public static final String DEFAULT_BASE_IMAGE = "ubuntu:22.04";

    public static final String ARM_ARCH = "aarch64";
    public static final String X86_64_ARCH = "amd64";

    private static final EnumSet<PosixFilePermission> POSIX_FILE_PERMISSIONS = EnumSet.of(
        PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE, PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.OTHERS_READ, PosixFilePermission.OTHERS_EXECUTE
    );
    private final MavenReaderFilter mavenReaderFilter;

    /**
     * The command to execute to determine if the application is ready to receive traffic.
     */
    @Parameter(property = DockerCracMojo.CRAC_READINESS_PROPERTY, defaultValue = DockerCracMojo.DEFAULT_READINESS_COMMAND)
    private String readinessCommand;

    /**
     * The timeout in seconds to wait for the checkpoint to complete.
     */
    @Parameter(property = DockerCracMojo.CRAC_CHECKPOINT_TIMEOUT_PROPERTY, defaultValue = DockerCracMojo.DEFAULT_CRAC_CHECKPOINT_TIMEOUT)
    private Integer checkpointTimeoutSeconds;

    /**
     * The name of the docker network to run the checkpoint container in.
     */
    @Parameter(property = DockerCracMojo.CRAC_CHECKPOINT_NETWORK_PROPERTY)
    private String checkpointNetworkName;

    /**
     * The version of the Azul CRaC enabled JDK to use.
     */
    @Parameter(property = DockerCracMojo.CRAC_JAVA_VERSION, defaultValue = "${jdk.version}")
    private String cracJavaVersion;

    /**
     * The architecture to use for the CRaC enabled JDK. Defaults to {@code os.arch}
     */
    @Parameter(property = DockerCracMojo.CRAC_ARCHITECTURE)
    private String cracArchitecture;

    /**
     * The os to use for the CRaC enabled JDK.
     */
    @Parameter(property = DockerCracMojo.CRAC_OS, defaultValue = DEFAULT_CRAC_OS)
    private String cracOs;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerCracMojo(
        MavenProject mavenProject,
        JibConfigurationService jibConfigurationService,
        ApplicationConfigurationService applicationConfigurationService,
        DockerService dockerService,
        MavenReaderFilter mavenReaderFilter,
        MavenSession mavenSession,
        MojoExecution mojoExecution
    ) {
        super(mavenProject, jibConfigurationService, applicationConfigurationService, dockerService, mavenSession, mojoExecution);
        this.mavenReaderFilter = mavenReaderFilter;
    }

    @Override
    public void execute() throws MojoExecutionException {
        try {
            copyDependencies();

            MicronautRuntime runtime = MicronautRuntime.valueOf(micronautRuntime.toUpperCase());

            switch (runtime.getBuildStrategy()) {
                case LAMBDA -> throw new MojoExecutionException("Lambda Functions are currently unsupported");
                case ORACLE_FUNCTION -> throw new MojoExecutionException("Oracle Functions are currently unsupported");
                case DEFAULT -> buildDockerCrac();
                default -> throw new IllegalStateException("Unexpected value: " + runtime.getBuildStrategy());
            }
        } catch (InvalidImageReferenceException iire) {
            String message = "Invalid image reference "
                + iire.getInvalidReference()
                + ", perhaps you should check that the reference is formatted correctly according to " +
                "https://docs.docker.com/engine/reference/commandline/tag/#extended-description" +
                "\nFor example, slash-separated name components cannot have uppercase letters";
            throw new MojoExecutionException(message);
        } catch (IOException | IllegalArgumentException | MavenFilteringException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void buildDockerCrac() throws IOException, InvalidImageReferenceException, MavenFilteringException {
        String checkpointImage = buildCheckpointDockerfile();
        getLog().info("CRaC Checkpoint image: " + checkpointImage);
        File checkpointDir = new File(mavenProject.getBuild().getDirectory(), "cr");
        // We need to make this folder first, or else Docker on linux will make it as root and break clean on CI
        checkpointDir.mkdirs();
        dockerService.runPrivilegedImageAndWait(
            checkpointImage,
            checkpointTimeoutSeconds,
            checkpointNetworkName,
            checkpointDir.getAbsolutePath() + ":/home/app/cr"
        );
        buildFinalDockerfile(checkpointImage);
    }

    private String limitArchitecture(String architecture) {
        if (architecture == null) {
            return null;
        }
        if (ARM_ARCH.equals(architecture)) {
            return architecture;
        }
        return X86_64_ARCH;
    }

    private String buildCheckpointDockerfile() throws IOException, MavenFilteringException {
        String name = mavenProject.getArtifactId() + "-crac-checkpoint";
        var checkpointTags = Collections.singleton(name);
        copyScripts(CHECKPOINT_SCRIPT_NAME, WARMUP_SCRIPT_NAME, RUN_SCRIPT_NAME);
        File dockerfile = dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_CRAC_CHECKPOINT);

        String systemArchitecture = limitArchitecture(System.getProperty("os.arch"));
        String filteredCracArchitecture = limitArchitecture(cracArchitecture);
        String finalArchitecture = filteredCracArchitecture == null ? systemArchitecture : filteredCracArchitecture;
        String baseImage = getFromImage().orElse(DEFAULT_BASE_IMAGE);

        getLog().info("Using BASE_IMAGE: " + baseImage);
        getLog().info("Using CRAC_ARCH: " + finalArchitecture);
        getLog().info("Using CRAC_JDK_VERSION: " + cracJavaVersion);
        getLog().info("Using CRAC_OS: " + cracOs);

        BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
            .withDockerfile(dockerfile)
            .withBuildArg("BASE_IMAGE", baseImage)
            .withBuildArg("CRAC_ARCH", finalArchitecture)
            .withBuildArg("CRAC_OS", cracOs)
            .withBuildArg("CRAC_JDK_VERSION", cracJavaVersion)
            .withTags(checkpointTags);
        getNetworkMode().ifPresent(buildImageCmd::withNetworkMode);
        dockerService.buildImage(buildImageCmd);
        return name;
    }

    private void buildFinalDockerfile(String checkpointContainerId) throws IOException, InvalidImageReferenceException, MavenFilteringException {
        Set<String> tags = getTags();
        for (String tag : tags) {
            ImageReference.parse(tag);
        }

        String ports = getPorts();
        getLog().info("Exposing port(s): " + ports);

        copyScripts(RUN_SCRIPT_NAME);
        File dockerfile = dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_CRAC);
        String baseImage = getFromImage().orElse(DEFAULT_BASE_IMAGE);

        getLog().info("Using BASE_IMAGE: " + baseImage);
        getLog().info("Using CHECKPOINT_IMAGE: " + checkpointContainerId);

        BuildImageCmd buildImageCmd = dockerService.buildImageCmd()
            .withDockerfile(dockerfile)
            .withBuildArg("PORT", ports)
            .withBuildArg("BASE_IMAGE", getFromImage().orElse(DEFAULT_BASE_IMAGE))
            .withBuildArg("CHECKPOINT_IMAGE", checkpointContainerId)
            .withTags(getTags());
        getNetworkMode().ifPresent(buildImageCmd::withNetworkMode);
        dockerService.buildImage(buildImageCmd);

        getLog().warn("**********************************************************");
        getLog().warn(" CRaC checkpoint files may contain sensitive information.");
        getLog().warn("**********************************************************");
    }

    private Properties replacementProperties(String readinessCommand, String mainClass) {
        Properties properties = new Properties();
        properties.setProperty("READINESS", readinessCommand);
        properties.setProperty("MAINCLASS", mainClass);
        return properties;
    }

    private void copyScripts(String... scriptNames) throws IOException, MavenFilteringException {
        var target = new File(mavenProject.getBuild().getDirectory(), "scripts");
        if (!target.exists()) {
            target.mkdirs();
        }
        processScripts(target, replacementProperties(readinessCommand, mainClass), scriptNames);
    }

    private void processScripts(File target, Properties replacements, String... scriptNames) throws IOException, MavenFilteringException {
        for (String script : scriptNames) {
            var localOverride = new File(mavenProject.getBasedir(), script);
            InputStream resourceStream = DockerCracMojo.class.getResourceAsStream("/cracScripts/" + script);
            Reader resourceReader = resourceStream == null ? null : new InputStreamReader(resourceStream);
            try (Reader reader = localOverride.exists() ? Files.newBufferedReader(localOverride.toPath()) : resourceReader) {
                if (reader == null) {
                    throw new IOException("Could not find script " + script);
                }
                var req = new MavenReaderFilterRequest();
                req.setFrom(reader);
                req.setFiltering(true);
                req.setAdditionalProperties(replacements);
                Path outputPath = target.toPath().resolve(script);
                try (Writer writer = Files.newBufferedWriter(outputPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
                    IOUtils.copy(mavenReaderFilter.filter(req), writer);
                }
                Files.setPosixFilePermissions(outputPath, POSIX_FILE_PERMISSIONS);
            }
        }
    }
}
