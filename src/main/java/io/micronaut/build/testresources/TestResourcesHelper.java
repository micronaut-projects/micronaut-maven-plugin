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
import io.micronaut.testresources.buildtools.*;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.resolution.DependencyResolutionException;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.build.MojoUtils.findJavaExecutable;
import static io.micronaut.build.services.DependencyResolutionService.testResourcesModuleToAetherArtifact;
import static io.micronaut.build.services.DependencyResolutionService.toClasspathFiles;
import static java.util.stream.Stream.concat;

/**
 * Utility class to stop Test Resources service.
 */
public class TestResourcesHelper {

    private static final String TEST_RESOURCES_PROPERTIES = "test-resources.properties";
    private static final String PORT_FILE_NAME = "test-resources-port.txt";

    private final boolean enabled;

    private final boolean keepAlive;

    private final boolean shared;

    private final File buildDirectory;

    private final Log log;

    private Integer explicitPort;

    private Integer clientTimeout;

    private MavenProject mavenProject;

    private MavenSession mavenSession;

    private DependencyResolutionService dependencyResolutionService;

    private ToolchainManager toolchainManager;

    private String testResourcesVersion;

    private boolean classpathInference;

    private List<Dependency> testResourcesDependencies;

    private String sharedServerNamespace;

    public TestResourcesHelper(boolean enabled, boolean keepAlive, boolean shared, File buildDirectory,
                               Integer explicitPort, Integer clientTimeout, MavenProject mavenProject,
                               MavenSession mavenSession, DependencyResolutionService dependencyResolutionService,
                               ToolchainManager toolchainManager, String testResourcesVersion,
                               boolean classpathInference, List<Dependency> testResourcesDependencies,
                               String sharedServerNamespace) {
        this(enabled, keepAlive, shared, buildDirectory);
        this.explicitPort = explicitPort;
        this.clientTimeout = clientTimeout;
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
        this.testResourcesVersion = testResourcesVersion;
        this.classpathInference = classpathInference;
        this.testResourcesDependencies = testResourcesDependencies;
        this.sharedServerNamespace = sharedServerNamespace;
    }

    public TestResourcesHelper(boolean enabled, boolean keepAlive, boolean shared, File buildDirectory) {
        this.enabled = enabled;
        this.keepAlive = keepAlive;
        this.shared = shared;
        this.buildDirectory = buildDirectory;
        this.log = new SystemStreamLog();
    }

    /**
     * Starts the Test Resources Service.
     */
    public void start() throws MojoExecutionException {
        if (!enabled) {
            return;
        }
        try {
            doStart();
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to start test resources server", e);
        }
    }

    private void doStart() throws DependencyResolutionException, IOException {
        if (shared) {
            if (sharedServerNamespace != null) {
                log.info("Test Resources is configured in shared mode with the namespace: " + sharedServerNamespace);
            } else {
                log.info("Test Resources is configured in shared mode");
            }
        }
        String accessToken = UUID.randomUUID().toString();
        Path buildDir = buildDirectory.toPath();
        Path serverSettingsDirectory = getServerSettingsDirectory();
        AtomicBoolean serverStarted = new AtomicBoolean(false);
        ServerUtils.startOrConnectToExistingServer(
                explicitPort,
                buildDir.resolve(PORT_FILE_NAME),
                serverSettingsDirectory,
                accessToken,
                resolveServerClasspath(),
                clientTimeout,
                new ServerFactory() {
                    Process process;

                    @Override
                    public void startServer(ServerUtils.ProcessParameters processParameters) throws IOException {
                        log.info("Starting Micronaut Test Resources service");
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
            log.info("Micronaut Test Resources service is started in the background. To stop it, run the following command: 'mvn mn:" + StopTestResourcesServerMojo.NAME + "'");
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

        return toClasspathFiles(dependencyResolutionService.artifactResultsFor(artifacts, true));
    }

    private List<MavenDependency> getApplicationDependencies() {
        return this.mavenProject.getDependencies().stream()
                .map(DependencyResolutionService::mavenDependencyToTestResourcesDependency)
                .collect(Collectors.toList());
    }

    /**
     * Contains the logic to stop the Test Resources Service.
     */
    public void stop() throws MojoExecutionException {
        if (!enabled || Boolean.TRUE.equals(keepAlive)) {
            return;
        }
        if (Files.exists(getKeepAliveFile())) {
            try {
                Files.delete(getKeepAliveFile());
            } catch (IOException e) {
                throw new MojoExecutionException("Failed to delete keepalive file", e);
            }
            return;
        }
        try {
            Optional<ServerSettings> optionalServerSettings = ServerUtils.readServerSettings(getServerSettingsDirectory());
            if (optionalServerSettings.isPresent() && ServerUtils.isServerStarted(optionalServerSettings.get().getPort())) {
                log.info("Shutting down Micronaut Test Resources service");
                doStop();
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to stop test resources server", e);
        }
    }

    private void doStop() throws IOException {
        Path settingsDirectory = getServerSettingsDirectory();
        ServerUtils.stopServer(settingsDirectory);
        Files.walkFileTree(settingsDirectory, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return super.visitFile(file, attrs);
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                Files.delete(dir);
                return super.postVisitDirectory(dir, exc);
            }
        });
    }

    private Path getServerSettingsDirectory() {
        if (shared) {
            return ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace);
        }
        return serverSettingsDirectoryOf(buildDirectory.toPath());
    }

    private Path getKeepAliveFile() {
        return getServerSettingsDirectory().resolve("keepalive");
    }

    private Path serverSettingsDirectoryOf(Path buildDir) {
        return buildDir.resolve("../.micronaut/test-resources");
    }

    /**
     * @param sharedServerNamespace The shared server namespace (if any).
     */
    public void setSharedServerNamespace(String sharedServerNamespace) {
        this.sharedServerNamespace = sharedServerNamespace;
    }
}
