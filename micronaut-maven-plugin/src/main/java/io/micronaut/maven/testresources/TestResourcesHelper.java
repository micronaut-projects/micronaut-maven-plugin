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
package io.micronaut.maven.testresources;

import io.micronaut.core.io.socket.SocketUtils;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.testresources.buildtools.MavenDependency;
import io.micronaut.testresources.buildtools.ModuleIdentifier;
import io.micronaut.testresources.buildtools.ServerFactory;
import io.micronaut.testresources.buildtools.ServerSettings;
import io.micronaut.testresources.buildtools.ServerUtils;
import io.micronaut.testresources.buildtools.TestResourcesClasspath;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

import static io.micronaut.maven.services.DependencyResolutionService.toClasspathFiles;
import static java.util.stream.Stream.concat;

/**
 * Utility class to stop Test Resources service.
 */
public class TestResourcesHelper {

    private static final String TEST_RESOURCES_PROPERTIES = "test-resources.properties";
    private static final String PORT_FILE_NAME = "test-resources-port.txt";

    private static final String TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX = "micronaut.test.resources.";

    private static final String TEST_RESOURCES_PROP_SERVER_URI = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.uri";
    private static final String TEST_RESOURCES_PROP_ACCESS_TOKEN = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.access.token";
    private static final String TEST_RESOURCES_PROP_CLIENT_READ_TIMEOUT = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.client.read.timeout";

    private final boolean enabled;

    private final MavenSession mavenSession;

    private final boolean shared;

    private final File buildDirectory;

    private final Log log;

    private Integer explicitPort;

    private Integer clientTimeout;

    private Integer serverIdleTimeoutMinutes;

    private MavenProject mavenProject;

    private DependencyResolutionService dependencyResolutionService;

    private ToolchainManager toolchainManager;

    private String testResourcesVersion;

    private boolean classpathInference;

    private List<Dependency> testResourcesDependencies;

    private String sharedServerNamespace;

    private boolean debugServer;

    public TestResourcesHelper(boolean enabled,
                               boolean shared,
                               File buildDirectory,
                               Integer explicitPort,
                               Integer clientTimeout,
                               Integer serverIdleTimeoutMinutes,
                               MavenProject mavenProject,
                               MavenSession mavenSession,
                               DependencyResolutionService dependencyResolutionService,
                               ToolchainManager toolchainManager,
                               String testResourcesVersion,
                               boolean classpathInference,
                               List<Dependency> testResourcesDependencies,
                               String sharedServerNamespace,
                               boolean debugServer) {
        this(mavenSession, enabled, shared, buildDirectory);
        this.explicitPort = explicitPort;
        this.clientTimeout = clientTimeout;
        this.serverIdleTimeoutMinutes = serverIdleTimeoutMinutes;
        this.mavenProject = mavenProject;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
        this.testResourcesVersion = testResourcesVersion;
        this.classpathInference = classpathInference;
        this.testResourcesDependencies = testResourcesDependencies;
        this.sharedServerNamespace = sharedServerNamespace;
        this.debugServer = debugServer;
    }

    public TestResourcesHelper(MavenSession mavenSession, boolean enabled, boolean shared, File buildDirectory) {
        this.mavenSession = mavenSession;
        this.enabled = enabled;
        this.shared = shared;
        this.buildDirectory = buildDirectory;
        this.log = new SystemStreamLog();
    }

    private boolean isKeepAlive() {
        boolean hasKeepAliveFile = Files.exists(getKeepAliveFile());
        return hasKeepAliveFile || isStartExplicitlyInvoked();
    }

    private boolean isStartExplicitlyInvoked() {
        return mavenSession.getGoals()
            .stream()
            .anyMatch(goal -> goal.equals("mn:" + StartTestResourcesServerMojo.NAME));
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

    private void doStart() throws IOException {
        var accessToken = UUID.randomUUID().toString();
        Path buildDir = buildDirectory.toPath();
        Path serverSettingsDirectory = getServerSettingsDirectory();
        var serverStarted = new AtomicBoolean(false);
        var serverFactory = new DefaultServerFactory(log, toolchainManager, mavenSession, serverStarted, testResourcesVersion, debugServer);
        Optional<ServerSettings> optionalServerSettings = startOrConnectToExistingServer(accessToken, buildDir, serverSettingsDirectory, serverFactory);
        if (optionalServerSettings.isPresent()) {
            ServerSettings serverSettings = optionalServerSettings.get();
            if (shared) {
                if (sharedServerNamespace != null) {
                    log.info("Test Resources is configured in shared mode with the namespace: " + sharedServerNamespace);
                    //Copy the server settings to the default location so that TR Client can find it
                    Path projectSettingsDirectory = serverSettingsDirectoryOf(buildDirectory.toPath());
                    Files.createDirectories(projectSettingsDirectory);

                    Path source = serverSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES);
                    Path target = projectSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES);
                    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                } else {
                    log.info("Test Resources is configured in shared mode");
                }
            }
            setSystemProperties(serverSettings);
            if (serverStarted.get()) {
                if (isKeepAlive()) {
                    log.info("Micronaut Test Resources service is started in the background. To stop it, run the following command: 'mvn mn:" + StopTestResourcesServerMojo.NAME + "'");
                }
            } else {
                // A server was already listening, which means it was running before
                // the build was started, so we put a file to indicate to the stop
                // mojo that it should not stop the server.
                Path keepalive = getKeepAliveFile();
                // Test is because we may be running in watch mode
                if (!Files.exists(keepalive)) {
                    Files.write(keepalive, "true".getBytes());
                    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                        // Make sure that if the build is interrupted, e.g using CTRL+C, the keepalive file is deleted
                        try {
                            deleteKeepAliveFile();
                        } catch (MojoExecutionException e) {
                            // ignore, we're in a shutdown hook
                        }
                    }));
                }
            }
        }
    }

    /**
     * Computes the system properties to set for the test resources client to be able to connect to the server.
     *
     * @param serverSettings The server settings
     * @return The system properties
     */
    public Map<String, String> computeSystemProperties(ServerSettings serverSettings) {
        var systemProperties = new HashMap<String, String>(3);
        String uri = String.format("http://localhost:%d", serverSettings.getPort());
        systemProperties.put(TEST_RESOURCES_PROP_SERVER_URI, uri);
        serverSettings.getAccessToken().ifPresent(accessToken -> systemProperties.put(TEST_RESOURCES_PROP_ACCESS_TOKEN, accessToken));
        serverSettings.getClientTimeout().ifPresent(timeout -> systemProperties.put(TEST_RESOURCES_PROP_CLIENT_READ_TIMEOUT, String.valueOf(timeout)));
        return systemProperties;
    }

    private void setSystemProperties(ServerSettings serverSettings) {
        computeSystemProperties(serverSettings).forEach(System::setProperty);
    }

    private Optional<ServerSettings> startOrConnectToExistingServer(String accessToken, Path buildDir, Path serverSettingsDirectory, ServerFactory serverFactory) {
        try {
            return Optional.ofNullable(
                ServerUtils.startOrConnectToExistingServer(
                    explicitPort,
                    buildDir.resolve(PORT_FILE_NAME),
                    serverSettingsDirectory,
                    accessToken,
                    resolveServerClasspath(),
                    clientTimeout,
                    serverIdleTimeoutMinutes,
                    serverFactory
                )
            );
        } catch (Exception e) {
            log.error("Error starting Micronaut Test Resources service", e);
            return Optional.empty();
        }
    }

    private List<File> resolveServerClasspath() throws DependencyResolutionException {
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

        Stream<Artifact> artifacts = concat(serverDependencies, extraDependenciesStream);

        var resolutionResult = dependencyResolutionService.artifactResultsFor(artifacts, true);
        var filteredArtifacts = resolutionResult.stream()
            .filter(result -> {
                var artifact = result.getArtifact();
                var id = new ModuleIdentifier(artifact.getGroupId(), artifact.getArtifactId());
                return TestResourcesClasspath.isDependencyAllowedOnServerClasspath(id);
            })
            .toList();
        return toClasspathFiles(filteredArtifacts);
    }

    private List<MavenDependency> getApplicationDependencies() {
        return this.mavenProject.getDependencies().stream()
            .map(DependencyResolutionService::mavenDependencyToTestResourcesDependency)
            .toList();
    }

    /**
     * Contains the logic to stop the Test Resources Service.
     *
     * @param quiet Whether to perform logging or not.
     */
    public void stop(boolean quiet) throws MojoExecutionException {
        if (!enabled) {
            return;
        }
        if (isKeepAlive()) {
            log("Keeping Micronaut Test Resources service alive", quiet);
            return;
        }
        try {
            Optional<ServerSettings> optionalServerSettings = ServerUtils.readServerSettings(getServerSettingsDirectory());
            if (optionalServerSettings.isPresent() && isServerStarted(optionalServerSettings.get().getPort())) {
                log("Shutting down Micronaut Test Resources service", quiet);
                doStop();
            } else {
                log("Cannot find Micronaut Test Resources service settings, server may already be shutdown", quiet);
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to stop test resources server", e);
        }
    }

    private static boolean isServerStarted(int port) {
        if (System.getProperty("test.resources.internal.server.started") != null) {
            return Boolean.getBoolean("test.resources.internal.server.started");
        } else {
            return !SocketUtils.isTcpPortAvailable(port);
        }
    }

    private void log(String message, boolean quiet) {
        if (quiet) {
            if (log.isDebugEnabled()) {
                log.debug(message);
            }
        } else {
            log.info(message);
        }
    }

    private void doStop() throws IOException, MojoExecutionException {
        try {
            Path settingsDirectory = getServerSettingsDirectory();
            ServerUtils.stopServer(settingsDirectory);
        } finally {
            deleteKeepAliveFile();
        }
    }

    private void deleteKeepAliveFile() throws MojoExecutionException {
        if (Files.exists(getKeepAliveFile())) {
            try {
                Files.delete(getKeepAliveFile());
            } catch (IOException e) {
                throw new MojoExecutionException("Failed to delete keepalive file", e);
            }
        }
    }

    private Path getServerSettingsDirectory() {
        if (shared) {
            return ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace);
        }
        return serverSettingsDirectoryOf(buildDirectory.toPath());
    }

    private Path getKeepAliveFile() {
        var tmpDir = Path.of(System.getProperty("java.io.tmpdir"));
        return tmpDir.resolve("keepalive-" + mavenSession.getRequest().getBuilderId());
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
