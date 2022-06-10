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
package io.micronaut.build.testresources;

import io.micronaut.build.services.DependencyResolutionService;
import io.micronaut.testresources.buildtools.MavenDependency;
import io.micronaut.testresources.buildtools.TestResourcesClasspath;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.resolution.DependencyResolutionException;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.build.MojoUtils.findJavaExecutable;
import static io.micronaut.build.services.DependencyResolutionService.*;
import static java.util.stream.Stream.concat;

/**
 * Starts the Micronaut test resources server.
 */
@Mojo(name = MicronautTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class MicronautTestResourcesServerMojo extends AbstractMojo {
    public static final String NAME = "start-testresources-server";

    private static final String SERVER_MAIN_CLASS = "io.micronaut.testresources.server.Application";

    private static final String DEFAULT_ENABLED = "false";
    private static final String DEFAULT_CLASSPATH_INFERENCE = "true";
    private static final String DEFAULT_CLIENT_TIMEOUT = "60";

    private static final String CONFIG_PROPERTY_PREFIX = "micronaut.test-resources.";

    /**
     * Whether to enable or disable Micronaut test resources support.
     */
    @Parameter(property =  "micronaut.test-resources.enabled", defaultValue = DEFAULT_ENABLED)
    private Boolean testResourcesEnabled = Boolean.valueOf(DEFAULT_ENABLED);

    /**
     * Micronaut Test Resources version. Should be defined by the Micronaut BOM, but this parameter can be used to
     * define a different version.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "version", required = true)
    private String testResourcesVersion;

    /**
     * If set to true, Micronaut will attempt to infer which dependencies should be added to the Test Resources server
     * classpath, based on the project dependencies. In general the result will consist of modules from the
     * test-resources project, but it may consist of additional entries, for example database drivers.
     */
    @Parameter(defaultValue = DEFAULT_CLASSPATH_INFERENCE)
    private Boolean classpathInference = Boolean.valueOf(DEFAULT_CLASSPATH_INFERENCE);

    /**
     * Additional dependencies to add to the Test Resources server classpath when not using classpath inference, or when
     * the inference doesn't produce the desired result.
     */
    @Parameter
    private List<Dependency> testResourcesDependencies;

    /**
     * By default, the Test Resources server will be started on a random (available) port, but it can be set a fixed port
     * by using this parameter.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "port")
    private Integer explicitPort;

    /**
     * Configures the maximum amount of time to wait for the server to start a test resource. Some containeres may take
     * a long amount of time to start with slow internet connections.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "client-timeout", defaultValue = DEFAULT_CLIENT_TIMEOUT)
    private Integer clientTimeout = Integer.valueOf(DEFAULT_CLIENT_TIMEOUT);

    @Parameter(defaultValue = "${project.build.directory}", required = true)
    private File buildDirectory;

    private final MavenProject mavenProject;

    private final MavenSession mavenSession;

    private final DependencyResolutionService dependencyResolutionService;

    private final ToolchainManager toolchainManager;

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public MicronautTestResourcesServerMojo(MavenProject mavenProject,
                                            MavenSession mavenSession,
                                            DependencyResolutionService dependencyResolutionService,
                                            ToolchainManager toolchainManager) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
    }

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (!testResourcesEnabled) {
            return;
        }
        try {
            doExecute();
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to start test resources server", e);
        }
    }

    private void doExecute() throws DependencyResolutionException, IOException {
        String accessToken = UUID.randomUUID().toString();
        String serverPort;
        if (explicitPort != null) {
            warnAboutPotentialSecurityIssue();
            serverPort = explicitPort.toString();
            if (shouldStartServer()) {
                startServer(null);
            }
        } else {
            Process process = startServer(accessToken);
            serverPort = determineServerPort(process);
        }

        writeClientProperties(accessToken, serverPort);
    }

    private boolean shouldStartServer() {
        try (Socket ignored = new Socket("localhost", explicitPort)) {
            getLog().info("Test resources service already started on port " + explicitPort);
            return false;
        } catch (IOException e) {
            return true;
        }
    }

    private void writeClientProperties(String accessToken, String serverPort) throws IOException {
        File propertiesFile = new File(buildDirectory, "test-classes/test-resources.properties");
        Path classesDir = propertiesFile.getParentFile().toPath();
        if (!Files.isDirectory(classesDir)) {
            Files.createDirectory(classesDir);
        }
        Properties properties = new Properties();
        properties.put("server.uri", "http://localhost:" + serverPort);
        properties.put("server.client.read.timeout", clientTimeout.toString());

        if (explicitPort == null) {
            properties.put("server.access.token", accessToken);
        }

        try (OutputStream out = Files.newOutputStream(propertiesFile.toPath())) {
            properties.store(out, "Test Resources client properties generated by the Micronaut Maven Plugin");
        }
    }

    private void warnAboutPotentialSecurityIssue() {
        getLog().warn("********************************************");
        getLog().warn("An explicit port was configured for the test resources server: it will accept any incoming connection from the loopback network interface");
        getLog().warn("********************************************");
    }

    private Process startServer(String accessToken) throws IOException, DependencyResolutionException {
        getLog().info("Starting Micronaut Test Resources server");
        String javaBin = findJavaExecutable(toolchainManager, mavenSession);
        List<String> commandLine = new ArrayList<>();
        commandLine.add(javaBin);
        List<String> classpath = createExecPluginConfig(accessToken);
        commandLine.addAll(classpath);
        ProcessBuilder builder = new ProcessBuilder(commandLine);
        Process process = builder.inheritIO().start();
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            getLog().info("Stopping Micronaut Test Resources server");
            process.destroy();
        }));
        return process;
    }

    private String determineServerPort(Process process) throws IOException {
        if (explicitPort != null) {
            return explicitPort.toString();
        } else {
            Path serverPortFile = buildDirectory.toPath().resolve(TEST_RESOURCES_GROUP + ".port");
            getLog().info("Waiting for Test Resources server to become available...");
            while (!Files.exists(serverPortFile)) {
                try {
                    process.waitFor(200, TimeUnit.MILLISECONDS);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
            return Files.readAllLines(serverPortFile).get(0);
        }
    }

    private List<String> createExecPluginConfig(String accessToken) throws DependencyResolutionException {
        List<String> serverClasspath = resolveServerClasspath();

        List<String> args = Stream.of(
                "-classpath",
                String.join(File.pathSeparator, serverClasspath),
                SERVER_MAIN_CLASS
        ).collect(Collectors.toList());

        if (accessToken != null) {
            args.add("-Dserver.access-token=" + accessToken);
        }
        if (explicitPort == null) {
            args.add("--port-file=" + buildDirectory.toPath().resolve(TEST_RESOURCES_GROUP + ".port"));
        } else {
            args.add("-Dmicronaut.server.port=" + explicitPort);
        }
        return args;
    }

    private List<String> resolveServerClasspath() throws DependencyResolutionException {
        Stream<Artifact> clientDependencies = Stream.of(testResourcesModuleToAetherArtifact("client", testResourcesVersion));

        List<MavenDependency> applicationDependencies = Collections.emptyList();
        if (classpathInference) {
            applicationDependencies = getApplicationDependencies();
        }
        Stream<Artifact> serverDependencies =
                TestResourcesClasspath.inferTestResourcesClasspath(applicationDependencies, testResourcesVersion)
                        .stream()
                        .map(DependencyResolutionService::testResourcesDependencyToAetherArtifact);

        List<org.apache.maven.model.Dependency> extraDependencies =
                testResourcesDependencies != null ? testResourcesDependencies : Collections.emptyList();

        Stream<Artifact> extraDependenciesStream = extraDependencies.stream()
                .map(DependencyResolutionService::mavenDependencyToAetherArtifact);

        Stream<Artifact> artifacts = concat(concat(clientDependencies, serverDependencies), extraDependenciesStream);

        return toClasspath(dependencyResolutionService.artifactResultsFor(artifacts));
    }

    private List<MavenDependency> getApplicationDependencies() {
        return this.mavenProject.getDependencies().stream()
                .map(DependencyResolutionService::mavenDependencyToTestResourcesDependency)
                .collect(Collectors.toList());
    }
}
