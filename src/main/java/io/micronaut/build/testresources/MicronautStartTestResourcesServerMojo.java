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
import io.micronaut.testresources.buildtools.ServerFactory;
import io.micronaut.testresources.buildtools.ServerUtils;
import io.micronaut.testresources.buildtools.TestResourcesClasspath;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.build.MojoUtils.findJavaExecutable;
import static io.micronaut.build.services.DependencyResolutionService.testResourcesModuleToAetherArtifact;
import static io.micronaut.build.services.DependencyResolutionService.toClasspathFiles;
import static java.util.stream.Stream.concat;

/**
 * Starts the Micronaut test resources server.
 */
@Mojo(name = MicronautStartTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class MicronautStartTestResourcesServerMojo extends AbstractTestResourcesMojo {
    public static final String NAME = "start-testresources-service";

    private static final String DEFAULT_CLASSPATH_INFERENCE = "true";
    private static final String DEFAULT_CLIENT_TIMEOUT = "60";

    private static final String TEST_RESOURCES_PROPERTIES = "test-resources.properties";

    /**
     * Whether to enable or disable Micronaut test resources support.
     */
    @Parameter(property =  "micronaut.test-resources.enabled", defaultValue = "false")
    private boolean testResourcesEnabled;

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

    private final MavenProject mavenProject;

    private final MavenSession mavenSession;

    private final DependencyResolutionService dependencyResolutionService;

    private final ToolchainManager toolchainManager;

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public MicronautStartTestResourcesServerMojo(MavenProject mavenProject,
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
        Path buildDir = buildDirectory.toPath();
        Path serverSettingsDirectory = getServerSettingsDirectory();
        AtomicBoolean serverStarted = new AtomicBoolean(false);
        ServerUtils.startOrConnectToExistingServer(
                explicitPort,
                buildDir.resolve("ts-port-file.txt"),
                serverSettingsDirectory,
                accessToken,
                resolveServerClasspath(),
                clientTimeout,
                new ServerFactory() {
                    Process process;

                    @Override
                    public void startServer(ServerUtils.ProcessParameters processParameters) throws IOException {
                        getLog().info("Starting Micronaut Test Resources server");
                        String javaBin = findJavaExecutable(toolchainManager, mavenSession);
                        List<String> cli = new ArrayList<>();
                        cli.add(javaBin);
                        cli.addAll(processParameters.getJvmArguments());
                        processParameters.getSystemProperties().forEach((key, value) -> cli.add("-D" + key + "=" + value));
                        cli.add("-cp");
                        cli.add(processParameters.getClasspath().stream().map(File::getAbsolutePath).collect(Collectors.joining(File.pathSeparator)));
                        cli.add(processParameters.getMainClass());
                        cli.addAll(processParameters.getArguments());
                        ProcessBuilder builder = new ProcessBuilder(cli);
                        process = builder.inheritIO().start();
                        serverStarted.set(true);
                    }

                    @Override
                    public void waitFor(Duration duration) throws InterruptedException {
                        if (process != null) {
                            process.waitFor(duration.toMillis(), TimeUnit.MILLISECONDS);
                        }
                    }
                }
        );
        if (keepAlive) {
            getLog().info("Micronaut Test Resources service is started in the background. To stop it, run the following command: 'mvn mn:" + MicronautStopTestResourcesServerMojo.NAME + "'");
        }
        if (!serverStarted.get()) {
            // A server was already listening, which means it was running before
            // the build was started, so we put a file to indicate to the stop
            // mojo that it should not stop the server.
            Path keepalive = getKeepAliveFile();
            Files.write(keepalive, "true".getBytes());
        }
        // In order for the test resources client to connect, we need
        // to copy the test resources file to the test classes directory
        copyServerSettingsToClasspath(buildDir, serverSettingsDirectory);
    }

    private void copyServerSettingsToClasspath(Path buildDir, Path serverSettingsDirectory) throws IOException {
        Path testClassesDir = buildDir.resolve("test-classes");
        if (!Files.exists(testClassesDir)) {
            Files.createDirectories(testClassesDir);
        }
        Files.copy(serverSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES), testClassesDir.resolve(TEST_RESOURCES_PROPERTIES), StandardCopyOption.REPLACE_EXISTING);
    }

    private List<File> resolveServerClasspath() throws DependencyResolutionException {
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

        return toClasspathFiles(dependencyResolutionService.artifactResultsFor(artifacts));
    }

    private List<MavenDependency> getApplicationDependencies() {
        return this.mavenProject.getDependencies().stream()
                .map(DependencyResolutionService::mavenDependencyToTestResourcesDependency)
                .collect(Collectors.toList());
    }
}
