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
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileAlreadyExistsException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

import static io.micronaut.maven.services.DependencyResolutionService.toClasspathFiles;
import static io.micronaut.testresources.buildtools.ServerUtils.PROPERTIES_FILE_NAME;
import static java.util.stream.Stream.concat;

/**
 * Utility class to stop Test Resources service.
 */
public class TestResourcesHelper {

    private static final String TEST_RESOURCES_PROPERTIES = PROPERTIES_FILE_NAME;
    private static final String PORT_FILE_NAME = "test-resources-port.txt";
    private static final String APPLICATION_TEST_PROPERTIES = "application-test.properties";
    private static final String TEST_RESOURCES_SCOPE_PROPERTY = "micronaut.test.resources.scope";
    private static final String SCOPE_PREFIX = "mvn";
    private static final String TEST_RESOURCES_REQUIREMENTS_ENTRIES_PATH = "/requirements/entries";
    private static final String ACCESS_TOKEN_HEADER = "Access-Token";
    private static final int SERVER_PROBE_TIMEOUT_MS = 1_000;

    private static final String TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX = "micronaut.test.resources.";

    private static final String TEST_RESOURCES_PROP_SERVER_URI = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.uri";
    private static final String TEST_RESOURCES_PROP_ACCESS_TOKEN = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.access.token";
    private static final String TEST_RESOURCES_PROP_CLIENT_READ_TIMEOUT = TEST_RESOURCES_CLIENT_SYSTEM_PROP_PREFIX + "server.client.read.timeout";
    private static final Object SESSION_STATE_MONITOR = new Object();
    private static final Map<MavenSession, SessionState> SESSION_STATES = new WeakHashMap<>();
    private static final Map<Path, ReentrantLock> SHARED_SERVER_LOCKS = new ConcurrentHashMap<>();

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

    private boolean foreground = false;

    private Map<String, String> testResourcesSystemProperties;

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
                               boolean debugServer,
                               boolean foreground, final Map<String, String> testResourcesSystemProperties) {
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
        this.foreground = foreground;
        this.testResourcesSystemProperties = testResourcesSystemProperties;
    }

    public TestResourcesHelper(MavenSession mavenSession, boolean enabled, boolean shared, File buildDirectory) {
        this.mavenSession = mavenSession;
        this.enabled = enabled;
        this.shared = shared;
        this.buildDirectory = buildDirectory;
        this.log = new SystemStreamLog();
    }

    private boolean isKeepAlive() {
        boolean hasKeepAliveFile = Files.isRegularFile(getKeepAliveFile(), LinkOption.NOFOLLOW_LINKS);
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
        var serverFactory = new DefaultServerFactory(log, toolchainManager, mavenSession, serverStarted, testResourcesVersion, debugServer, foreground, !isKeepAlive(), testResourcesSystemProperties);
        if (shared) {
            try (var ignored = sharedServerLock(serverSettingsDirectory)) {
                doStart(accessToken, buildDir, serverSettingsDirectory, serverFactory, serverStarted);
            }
            return;
        }
        doStart(accessToken, buildDir, serverSettingsDirectory, serverFactory, serverStarted);
    }

    private void doStart(String accessToken,
                         Path buildDir,
                         Path serverSettingsDirectory,
                         ServerFactory serverFactory,
                         AtomicBoolean serverStarted) throws IOException {
        Optional<ServerSettings> sessionSharedServerSettings = findSessionSharedServer(serverSettingsDirectory);
        if (sessionSharedServerSettings.isPresent()) {
            ServerSettings serverSettings = sessionSharedServerSettings.get();
            writePortFile(buildDir.resolve(PORT_FILE_NAME), serverSettings.getPort());
            registerSharedServerUse(serverSettingsDirectory, serverSettings.getPort(), false);
            logSharedMode(serverSettingsDirectory);
            writeSharedScopeConfiguration();
            setSystemProperties(serverSettings);
            return;
        }
        Optional<ServerSettings> existingServerSettings = findReachableRecordedServer(serverSettingsDirectory);
        if (existingServerSettings.isPresent()) {
            useServerSettings(buildDir, serverSettingsDirectory, existingServerSettings.get(), false);
            return;
        }
        Optional<ServerSettings> optionalServerSettings = startOrConnectToExistingServer(accessToken, buildDir, serverSettingsDirectory, serverFactory);
        if (optionalServerSettings.isEmpty()) {
            return;
        }
        ServerSettings serverSettings = optionalServerSettings.get();
        useServerSettings(buildDir, serverSettingsDirectory, serverSettings, serverStarted.get());
        if (serverStarted.get() && isKeepAlive()) {
            log.info("Micronaut Test Resources service is started in the background. To stop it, run the following command: 'mvn mn:" + StopTestResourcesServerMojo.NAME + "'");
        }
    }

    private Optional<ServerSettings> findSessionSharedServer(Path serverSettingsDirectory) {
        if (!shared || mavenProject == null) {
            return Optional.empty();
        }
        synchronized (SESSION_STATE_MONITOR) {
            SessionState sessionState = SESSION_STATES.get(mavenSession);
            if (sessionState == null) {
                return Optional.empty();
            }
            SharedServerState sharedServerState = sessionState.sharedServers.get(normalize(serverSettingsDirectory));
            if (sharedServerState == null) {
                return Optional.empty();
            }
            return ServerUtils.readServerSettings(serverSettingsDirectory)
                .filter(settings -> settings.getPort() == sharedServerState.port);
        }
    }

    private Optional<ServerSettings> findReachableRecordedServer(Path serverSettingsDirectory) {
        return ServerUtils.readServerSettings(serverSettingsDirectory)
            .filter(serverSettings -> explicitPort == null || serverSettings.getPort() == explicitPort)
            .filter(this::isReusableTestResourcesServer);
    }

    private boolean isReusableTestResourcesServer(ServerSettings serverSettings) {
        HttpURLConnection connection = null;
        try {
            var url = URI.create("http://localhost:" + serverSettings.getPort() + TEST_RESOURCES_REQUIREMENTS_ENTRIES_PATH).toURL();
            connection = (HttpURLConnection) url.openConnection();
            connection.setConnectTimeout(SERVER_PROBE_TIMEOUT_MS);
            connection.setReadTimeout(SERVER_PROBE_TIMEOUT_MS);
            connection.setRequestMethod("GET");
            connection.setRequestProperty("Accept", "application/json");
            String accessToken = serverSettings.getAccessToken().orElse(null);
            if (accessToken != null) {
                connection.setRequestProperty(ACCESS_TOKEN_HEADER, accessToken);
            }
            if (connection.getResponseCode() != HttpURLConnection.HTTP_OK || !isJsonContentType(connection.getContentType())) {
                return false;
            }
            try (InputStream input = connection.getInputStream()) {
                return isJsonStringArray(new String(input.readAllBytes(), StandardCharsets.UTF_8));
            }
        } catch (IOException _) {
            return false;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private static boolean isJsonContentType(String contentType) {
        if (contentType == null) {
            return false;
        }
        return contentType.equals("application/json") || contentType.startsWith("application/json;");
    }

    private static boolean isJsonStringArray(String body) {
        String trimmed = body.trim();
        return trimmed.length() >= 2 && trimmed.charAt(0) == '[' && trimmed.charAt(trimmed.length() - 1) == ']';
    }

    private void useServerSettings(Path buildDir, Path serverSettingsDirectory, ServerSettings serverSettings, boolean serverStarted) throws IOException {
        writePortFile(buildDir.resolve(PORT_FILE_NAME), serverSettings.getPort());
        boolean sessionOwnedSharedServer = shared && registerSharedServerUse(serverSettingsDirectory, serverSettings.getPort(), serverStarted);
        if (shared) {
            logSharedMode(serverSettingsDirectory);
            writeSharedScopeConfiguration();
        }
        setSystemProperties(serverSettings);
        if (!serverStarted && !sessionOwnedSharedServer) {
            // A server was already listening before this build started, so leave it running.
            createKeepAliveFile();
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

    private static void writePortFile(Path file, int port) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, Integer.toString(port));
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
        if (shared) {
            try (var ignored = sharedServerLock(getServerSettingsDirectory())) {
                stopSharedServer(quiet);
            }
            return;
        }
        stopServer(quiet);
    }

    private void stopSharedServer(boolean quiet) throws MojoExecutionException {
        if (releaseSharedServerUse(getServerSettingsDirectory())) {
            try {
                cleanupSharedProjectSettings();
            } catch (IOException e) {
                throw new MojoExecutionException("Unable to clean shared test resources settings", e);
            }
            log("Keeping Micronaut Test Resources service alive for another parallel reactor module", quiet);
            return;
        }
        stopServer(quiet);
    }

    private void stopServer(boolean quiet) throws MojoExecutionException {
        if (isKeepAlive()) {
            log("Keeping Micronaut Test Resources service alive", quiet);
            return;
        }
        try {
            Optional<ServerSettings> optionalServerSettings = ServerUtils.readServerSettings(getServerSettingsDirectory());
            if (optionalServerSettings.isPresent()) {
                if (isServerStarted(optionalServerSettings.get().getPort())) {
                    log("Shutting down Micronaut Test Resources service", quiet);
                    doStop();
                } else {
                    log("Cannot find Micronaut Test Resources service settings, server may already be shutdown", quiet);
                    Files.deleteIfExists(getServerSettingsDirectory().resolve(PROPERTIES_FILE_NAME));
                }
                cleanupSharedProjectSettings();
            }
        } catch (Exception e) {
            var message = "Unable to stop test resources server";
            if (quiet) {
                log.warn(message, e);
            } else {
                throw new MojoExecutionException(message, e);
            }
        }
    }

    private void logSharedMode(Path serverSettingsDirectory) throws IOException {
        if (sharedServerNamespace != null) {
            log.info("Test Resources is configured in shared mode with the namespace: " + sharedServerNamespace);
            Path projectSettingsDirectory = serverSettingsDirectoryOf(buildDirectory.toPath());
            Files.createDirectories(projectSettingsDirectory);
            Path source = serverSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES);
            Path target = projectSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES);
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        } else {
            log.info("Test Resources is configured in shared mode");
        }
    }

    private void writeSharedScopeConfiguration() throws IOException {
        String scope = sharedScope();
        if (scope == null) {
            return;
        }
        Path testClassesDirectory = testOutputDirectory(mavenProject, buildDirectory);
        Files.createDirectories(testClassesDirectory);
        updateApplicationTestProperties(testClassesDirectory.resolve(APPLICATION_TEST_PROPERTIES), scope);
        log.info("Using Micronaut Test Resources scope " + scope + " for " + moduleKey());
    }

    static Path testOutputDirectory(MavenProject mavenProject, File buildDirectory) {
        if (mavenProject != null && mavenProject.getBuild() != null) {
            String testOutputDirectory = mavenProject.getBuild().getTestOutputDirectory();
            if (testOutputDirectory != null && !testOutputDirectory.isBlank()) {
                return Path.of(testOutputDirectory);
            }
        }
        return buildDirectory.toPath().resolve("test-classes");
    }

    static void updateApplicationTestProperties(Path file, String scope) throws IOException {
        Properties properties = new Properties();
        if (Files.exists(file)) {
            try (InputStream input = Files.newInputStream(file)) {
                properties.load(input);
            }
        }
        properties.setProperty(TEST_RESOURCES_SCOPE_PROPERTY, scope);
        try (OutputStream output = Files.newOutputStream(file)) {
            properties.store(output, "Generated by micronaut-maven-plugin");
        }
    }

    static String sanitizeScopeSegment(String value) {
        String sanitized = value.replaceAll("[/\\\\]+", ".")
            .replaceAll("[^A-Za-z0-9_.-]", "-")
            .replaceAll("[.]{2,}", ".")
            .replaceAll("-{2,}", "-");
        sanitized = trimScopeDelimiters(sanitized);
        return sanitized.isEmpty() ? "root" : sanitized;
    }

    private static String trimScopeDelimiters(String value) {
        int start = 0;
        int end = value.length();
        while (start < end && isScopeDelimiter(value.charAt(start))) {
            start++;
        }
        while (end > start && isScopeDelimiter(value.charAt(end - 1))) {
            end--;
        }
        return value.substring(start, end);
    }

    private static boolean isScopeDelimiter(char c) {
        return c == '.' || c == '-';
    }

    private String sharedScope() {
        if (!shared || mavenProject == null) {
            return null;
        }
        Path multiModuleDirectory = mavenSession.getRequest().getMultiModuleProjectDirectory() == null
            ? null
            : mavenSession.getRequest().getMultiModuleProjectDirectory().toPath().toAbsolutePath().normalize();
        Path projectDirectory = mavenProject.getBasedir() == null
            ? null
            : mavenProject.getBasedir().toPath().toAbsolutePath().normalize();
        String projectSegment = mavenProject.getArtifactId();
        if (multiModuleDirectory != null && projectDirectory != null && projectDirectory.startsWith(multiModuleDirectory)) {
            Path relativePath = multiModuleDirectory.relativize(projectDirectory);
            if (relativePath.getNameCount() > 0) {
                projectSegment = relativePath.toString();
            }
        }
        return sessionState().scopePrefix + "." + sanitizeScopeSegment(projectSegment);
    }

    private boolean registerSharedServerUse(Path serverSettingsDirectory, int port, boolean serverStarted) {
        if (mavenProject == null) {
            return false;
        }
        synchronized (SESSION_STATE_MONITOR) {
            Path key = normalize(serverSettingsDirectory);
            SessionState sessionState = sessionState();
            SharedServerState sharedServerState = sessionState.sharedServers.get(key);
            if (serverStarted) {
                sharedServerState = new SharedServerState(port);
                sessionState.sharedServers.put(key, sharedServerState);
            } else if (sharedServerState == null || sharedServerState.port != port) {
                return false;
            }
            sharedServerState.owners.add(moduleKey());
            return true;
        }
    }

    private boolean releaseSharedServerUse(Path serverSettingsDirectory) {
        if (mavenProject == null) {
            return false;
        }
        synchronized (SESSION_STATE_MONITOR) {
            SessionState sessionState = SESSION_STATES.get(mavenSession);
            if (sessionState == null) {
                return false;
            }
            SharedServerState sharedServerState = sessionState.sharedServers.get(normalize(serverSettingsDirectory));
            if (sharedServerState == null) {
                return false;
            }
            sharedServerState.owners.remove(moduleKey());
            if (!sharedServerState.owners.isEmpty()) {
                return true;
            }
            sessionState.sharedServers.remove(normalize(serverSettingsDirectory));
            return false;
        }
    }

    private SessionState sessionState() {
        synchronized (SESSION_STATE_MONITOR) {
            return SESSION_STATES.computeIfAbsent(mavenSession, ignored -> new SessionState());
        }
    }

    private String moduleKey() {
        if (mavenProject == null || mavenProject.getBasedir() == null) {
            return buildDirectory.toPath().toAbsolutePath().normalize().toString();
        }
        return mavenProject.getBasedir().toPath().toAbsolutePath().normalize().toString();
    }

    private static Path normalize(Path path) {
        return path.toAbsolutePath().normalize();
    }

    static SharedServerLock sharedServerLock(Path serverSettingsDirectory) {
        Path key = normalize(serverSettingsDirectory);
        ReentrantLock lock = SHARED_SERVER_LOCKS.computeIfAbsent(key, ignored -> new ReentrantLock());
        return new SharedServerLock(key, lock);
    }

    static int sharedServerLockCount() {
        return SHARED_SERVER_LOCKS.size();
    }

    private static void releaseSharedServerLock(Path key, ReentrantLock lock) {
        lock.unlock();
        if (!lock.isLocked() && !lock.hasQueuedThreads()) {
            SHARED_SERVER_LOCKS.remove(key, lock);
        }
    }

    private void createKeepAliveFile() throws IOException {
        Path keepalive = getKeepAliveFile();
        createKeepAliveDirectory();
        if (Files.exists(keepalive, LinkOption.NOFOLLOW_LINKS)) {
            if (!Files.isRegularFile(keepalive, LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("Keepalive path exists but is not a regular file: " + keepalive);
            }
        } else {
            Files.writeString(keepalive, "true", StandardOpenOption.CREATE_NEW);
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                try {
                    deleteKeepAliveFile();
                } catch (MojoExecutionException e) {
                    // ignore, we're in a shutdown hook
                }
            }));
        }
    }

    private void createKeepAliveDirectory() throws IOException {
        synchronized (SESSION_STATE_MONITOR) {
            Path keepAliveDirectory = sessionState().keepAliveDirectory;
            if (Files.exists(keepAliveDirectory, LinkOption.NOFOLLOW_LINKS)) {
                if (!Files.isDirectory(keepAliveDirectory, LinkOption.NOFOLLOW_LINKS)) {
                    throw new IOException("Keepalive directory exists but is not a directory: " + keepAliveDirectory);
                }
                return;
            }
            try {
                FileAttribute<?>[] attributes = keepAliveDirectoryAttributes(keepAliveDirectory.getParent());
                if (attributes.length == 0) {
                    Files.createDirectory(keepAliveDirectory);
                } else {
                    Files.createDirectory(keepAliveDirectory, attributes);
                }
            } catch (FileAlreadyExistsException e) {
                if (!Files.isDirectory(keepAliveDirectory, LinkOption.NOFOLLOW_LINKS)) {
                    throw new IOException("Keepalive directory exists but is not a directory: " + keepAliveDirectory, e);
                }
            }
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
        if (Files.exists(getKeepAliveFile(), LinkOption.NOFOLLOW_LINKS)) {
            try {
                Files.delete(getKeepAliveFile());
            } catch (IOException e) {
                throw new MojoExecutionException("Failed to delete keepalive file", e);
            }
        }
        tryDeleteKeepAliveDirectory();
    }

    private Path getServerSettingsDirectory() {
        if (shared) {
            return ServerUtils.getDefaultSharedSettingsPath(sharedServerNamespace);
        }
        return serverSettingsDirectoryOf(buildDirectory.toPath());
    }

    private Path getKeepAliveFile() {
        return sessionState().keepAliveDirectory.resolve("keepalive-" + mavenSession.getRequest().getBuilderId());
    }

    private void tryDeleteKeepAliveDirectory() throws MojoExecutionException {
        Path keepAliveDirectory = sessionState().keepAliveDirectory;
        try {
            Files.deleteIfExists(keepAliveDirectory);
        } catch (DirectoryNotEmptyException ignored) {
            // Another keepalive file in the same session still owns the directory.
        } catch (IOException e) {
            throw new MojoExecutionException("Failed to delete keepalive directory", e);
        }
    }

    @SuppressWarnings("unchecked")
    private static FileAttribute<?>[] keepAliveDirectoryAttributes(Path directory) {
        try {
            if (directory == null || !Files.getFileStore(directory).supportsFileAttributeView("posix")) {
                return new FileAttribute[0];
            }
            return new FileAttribute<?>[]{
                PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
            };
        } catch (IOException | UnsupportedOperationException | SecurityException e) {
            return new FileAttribute[0];
        }
    }

    private void cleanupSharedProjectSettings() throws IOException {
        if (shared && sharedServerNamespace != null) {
            Path projectSettingsDirectory = serverSettingsDirectoryOf(buildDirectory.toPath());
            Files.deleteIfExists(projectSettingsDirectory.resolve(TEST_RESOURCES_PROPERTIES));
        }
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

    private static final class SessionState {
        private final String scopePrefix = SCOPE_PREFIX + "-" + UUID.randomUUID();
        private final Path keepAliveDirectory = Path.of(System.getProperty("java.io.tmpdir"))
            .resolve("mn-test-resources-" + UUID.randomUUID());
        private final Map<Path, SharedServerState> sharedServers = new LinkedHashMap<>();
    }

    private static final class SharedServerState {
        private final int port;
        private final Set<String> owners = new HashSet<>();

        private SharedServerState(int port) {
            this.port = port;
        }
    }

    static final class SharedServerLock implements AutoCloseable {
        private final Path key;
        private final ReentrantLock lock;

        private SharedServerLock(Path key, ReentrantLock lock) {
            this.key = key;
            this.lock = lock;
            this.lock.lock();
        }

        @Override
        public void close() {
            releaseSharedServerLock(key, lock);
        }
    }
}
