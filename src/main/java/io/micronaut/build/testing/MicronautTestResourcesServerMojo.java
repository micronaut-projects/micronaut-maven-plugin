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
package io.micronaut.build.testing;

import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.DependencyResolutionService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.resolution.DependencyResolutionException;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.build.MojoUtils.findJavaExecutable;
import static io.micronaut.build.services.DependencyResolutionService.toClasspath;
import static java.util.stream.Stream.concat;

/**
 * Starts the Micronaut test resources server.
 */
@Mojo(name = MicronautTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class MicronautTestResourcesServerMojo extends AbstractMojo {
    public static final String NAME = "start-testresources-server";

    private static final String SERVER_MAIN_CLASS = "io.micronaut.testresources.server.Application";
    private static final String TEST_RESOURCES_GROUP = "io.micronaut.testresources";
    private static final String TEST_RESOURCES_ARTIFACT_ID_PREFIX = "micronaut-test-resources-";
    private static final String TEST_RESOURCES_VERSION = "1.0.0-SNAPSHOT";
    private static final String[] TEST_RESOURCES_MODULES = new String[]{
            "testcontainers",
            "server",
            "hivemq",
            "jdbc-mariadb",
            "jdbc-mysql",
            "jdbc-oracle-xe",
            "jdbc-postgresql",
            "kafka",
            "mongodb",
            "neo4j"
    };


    /**
     * Whether to enable or disable Micronaut test resources support.
     */
    @Parameter(property = "micronaut.test-resources.enabled", defaultValue = "true")
    protected boolean enabled;

    @Parameter(defaultValue = "${project.build.directory}", required = true)
    protected File buildDirectory;

    @Parameter
    protected List<org.apache.maven.model.Dependency> testResourcesDependencies;

    protected final CompilerService compilerService;

    protected final MavenProject mavenProject;

    protected final MavenSession mavenSession;

    protected final RepositorySystem repositorySystem;

    protected final DependencyResolutionService dependencyResolutionService;

    protected final ToolchainManager toolchainManager;

    @Inject
    public MicronautTestResourcesServerMojo(CompilerService compilerService,
                                            MavenProject mavenProject,
                                            MavenSession mavenSession,
                                            RepositorySystem repositorySystem,
                                            DependencyResolutionService dependencyResolutionService,
                                            ToolchainManager toolchainManager) {
        this.compilerService = compilerService;
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.repositorySystem = repositorySystem;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
    }

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (!enabled) {
            return;
        }
        try {
            doExecute();
        } catch (IOException e) {
            e.printStackTrace();
            throw new MojoExecutionException("Unable to start test resources server", e);
        }
    }

    private void doExecute() throws IOException {
        getLog().info("Starting Micronaut Test Resources server");
        String javaBin = findJavaExecutable(toolchainManager, mavenSession);
        List<String> commandLine = new ArrayList<>();
        commandLine.add(javaBin);
        try {
            List<String> classpath = createExecPluginConfig();
            commandLine.addAll(classpath);
            ProcessBuilder builder = new ProcessBuilder(commandLine);
            Process process = builder.inheritIO().start();
            File propertiesFile = new File(buildDirectory, "test-classes/test-resources.properties");
            Path classesDir = propertiesFile.getParentFile().toPath();
            if (!Files.isDirectory(classesDir)) {
                Files.createDirectory(classesDir);
            }
            Path serverPortFile = buildDirectory.toPath().resolve(TEST_RESOURCES_GROUP + ".port");
            while (!Files.exists(serverPortFile)) {
                getLog().info("Waiting for Test Resources server to become available...");
                process.waitFor(500, TimeUnit.MILLISECONDS);
            }
            String serverPort = Files.readAllLines(serverPortFile).get(0);
            try (PrintWriter prn = new PrintWriter(Files.newOutputStream(propertiesFile.toPath()))) {
                prn.println("server.uri=http\\://localhost\\:" + serverPort);
            }
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                getLog().info("Stopping Micronaut Test Resources server");
                process.destroy();
            }));
        } catch (DependencyResolutionException | IOException | InterruptedException e) {
            getLog().error(e.getMessage());
            throw new RuntimeException(e);
        }

    }

    private List<String> createExecPluginConfig() throws DependencyResolutionException {
        List<String> serverClasspath = resolveServerClasspath();

        return Stream.of(
                "-classpath",
                String.join(File.pathSeparator, serverClasspath),
                SERVER_MAIN_CLASS,

                // CLI args
                "-Dmicronaut.http.client.read-timeout=60s",
                "--port-file=" + buildDirectory.toPath().resolve(TEST_RESOURCES_GROUP + ".port")
        ).collect(Collectors.toList());
    }

    private List<String> resolveServerClasspath() throws DependencyResolutionException {
        List<org.apache.maven.model.Dependency> extraDependencies =
                testResourcesDependencies != null ? testResourcesDependencies : Collections.emptyList();
        Stream<Artifact> artifacts = concat(
                Arrays.stream(TEST_RESOURCES_MODULES)
                        .map(m -> new DefaultArtifact(TEST_RESOURCES_GROUP + ":" + TEST_RESOURCES_ARTIFACT_ID_PREFIX + m + ":" + TEST_RESOURCES_VERSION)),
        extraDependencies.stream().map(d -> new DefaultArtifact(
                d.getGroupId(),
                d.getArtifactId(),
                d.getType(),
                d.getVersion()
        )));
        return toClasspath(dependencyResolutionService.artifactResultsFor(artifacts));
    }

}
