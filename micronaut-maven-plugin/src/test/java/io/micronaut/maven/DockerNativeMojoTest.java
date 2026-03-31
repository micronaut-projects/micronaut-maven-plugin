package io.micronaut.maven;

import com.github.dockerjava.api.command.BuildImageCmd;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.model.Build;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junitpioneer.jupiter.ClearSystemProperty;
import org.junitpioneer.jupiter.RestoreSystemProperties;
import org.junitpioneer.jupiter.SetSystemProperty;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

import static io.micronaut.maven.AbstractDockerMojo.X86_64_ARCH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
@ClearSystemProperty(key = "http.nonProxyHosts")
class DockerNativeMojoTest {

    @ParameterizedTest
    @CsvSource({
            "24,https://gds.oracle.com/download/graal/25/latest-gftc/graalvm-jdk-25_linux-x64_bin.tar.gz",
            "25,https://gds.oracle.com/download/graal/25/latest-gftc/graalvm-jdk-25_linux-x64_bin.tar.gz",
            "26,https://gds.oracle.com/download/graal/25/latest-gftc/graalvm-jdk-25_linux-x64_bin.tar.gz"
    })
    @SetSystemProperty(key = "os.arch", value = X86_64_ARCH)
    void testGraalVmDownloadUrl(String javaVersion, String expectedUrl) throws URISyntaxException, IOException, InterruptedException {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        var properties = new Properties(1);
        properties.put("maven.compiler.target", javaVersion);

        when(session.getCurrentProject()).thenReturn(project);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(properties);

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        var actualUrl = mojo.graalVmDownloadUrl();

        assertEquals(expectedUrl, actualUrl);

        var client = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NORMAL)
                .build();
        var request = HttpRequest.newBuilder()
                .uri(new URI(actualUrl))
                .method("HEAD", HttpRequest.BodyPublishers.noBody())
                .build();
        var response = client.send(request, HttpResponse.BodyHandlers.discarding());

        assertEquals(200, response.statusCode());
    }

    @Test
    @SetSystemProperty(key = "os.arch", value = X86_64_ARCH)
    void testGraalVmDownloadUrlFromReleaseVersion() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        var properties = new Properties(1);
        properties.put("maven.compiler.release", "25");

        when(session.getCurrentProject()).thenReturn(project);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(properties);

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        var actualUrl = mojo.graalVmDownloadUrl();

        var expectedUrl = "https://gds.oracle.com/download/graal/25/latest-gftc/graalvm-jdk-25_linux-x64_bin.tar.gz";
        assertEquals(expectedUrl, actualUrl);
    }

    @Test
    void testGetPortsFromJib() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8081"));
        var mojo = new DockerNativeMojo(project, jibConfigurationService, null, null, session, execution);

        var ports = mojo.getPorts();

        assertEquals("8081", ports);
    }

    @Test
    void testGetPortsFromAppConfig() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getPorts()).thenReturn(Optional.empty());
        var applicationConfigurationService = mock(ApplicationConfigurationService.class);
        when(applicationConfigurationService.getServerPort()).thenReturn("8081");
        var mojo = new DockerNativeMojo(project, jibConfigurationService, applicationConfigurationService, null, session, execution);

        var ports = mojo.getPorts();

        assertEquals("8081", ports);
    }

    @Test
    void testGetProxyBuildArgsWithAllProxiesSet() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);
        
        // Set system properties for proxy configuration
        System.setProperty("http.proxyHost", "proxy.example.com");
        System.setProperty("http.proxyPort", "8080");
        System.setProperty("https.proxyHost", "proxy.example.com");
        System.setProperty("https.proxyPort", "8080");
        System.setProperty("http.nonProxyHosts", "localhost|127.0.0.1|*.example.com");
        
        try {
            var proxyArgs = mojo.getProxyBuildArgs();

            assertEquals("http://proxy.example.com:8080", proxyArgs.get("HTTP_PROXY"));
            assertEquals("http://proxy.example.com:8080", proxyArgs.get("http_proxy"));
            assertEquals("http://proxy.example.com:8080", proxyArgs.get("HTTPS_PROXY"));
            assertEquals("http://proxy.example.com:8080", proxyArgs.get("https_proxy"));
            assertEquals("localhost,127.0.0.1,*.example.com", proxyArgs.get("NO_PROXY"));
            assertEquals("localhost,127.0.0.1,*.example.com", proxyArgs.get("no_proxy"));
            assertEquals(6, proxyArgs.size());
        } finally {
            // Clean up system properties
            System.clearProperty("http.proxyHost");
            System.clearProperty("http.proxyPort");
            System.clearProperty("https.proxyHost");
            System.clearProperty("https.proxyPort");
            System.clearProperty("http.nonProxyHosts");
        }
    }

    @Test
    void testGetProxyBuildArgsWithNoProxiesSet() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        var proxyArgs = mojo.getProxyBuildArgs();

        assertEquals(0, proxyArgs.size());
    }

    @Test
    void testGetProxyBuildArgsWithPartialProxiesSet() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);
        
        // Set only HTTP proxy using system properties
        System.setProperty("http.proxyHost", "proxy.example.com");
        System.setProperty("http.proxyPort", "8080");
        
        try {
            var proxyArgs = mojo.getProxyBuildArgs();

            assertEquals("http://proxy.example.com:8080", proxyArgs.get("HTTP_PROXY"));
            assertEquals("http://proxy.example.com:8080", proxyArgs.get("http_proxy"));
            assertEquals(2, proxyArgs.size());
        } finally {
            // Clean up system properties
            System.clearProperty("http.proxyHost");
            System.clearProperty("http.proxyPort");
        }
    }

    @Test
    void testGetProxyBuildArgsWithDefaultPorts() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        // Set proxy hosts without ports to test defaults
        System.setProperty("http.proxyHost", "proxy.example.com");
        System.setProperty("https.proxyHost", "proxy.example.com");

        try {
            var proxyArgs = mojo.getProxyBuildArgs();

            assertEquals("http://proxy.example.com:80", proxyArgs.get("HTTP_PROXY"));
            assertEquals("http://proxy.example.com:80", proxyArgs.get("http_proxy"));
            assertEquals("http://proxy.example.com:443", proxyArgs.get("HTTPS_PROXY"));
            assertEquals("http://proxy.example.com:443", proxyArgs.get("https_proxy"));
            assertEquals(4, proxyArgs.size());
        } finally {
            // Clean up system properties
            System.clearProperty("http.proxyHost");
            System.clearProperty("https.proxyHost");
        }
    }

    @ParameterizedTest
    @CsvSource({
            //staticNativeImage,    oracleLinuxVersion, jvmVersion, expectedTag
            "true,                  ol9,                21,         21-muslib-ol9",
            "true,                  '',                 21,         21-muslib",
            "false,                 ol9,                21,         21-ol9",
            ",                      ol9,                21,         21-ol9",
            "false,                 '',                 21,         21",
            ",                      ,                   21,         21",
            ",                      ,                   25,         25"
    })
    void testGraalVmTag(Boolean staticNativeImage, String oracleLinuxVersion, String jvmVersion, String expectedTag) {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        var actualTag = mojo.graalVmTag(jvmVersion, staticNativeImage, oracleLinuxVersion);

        assertEquals(expectedTag, actualTag);
    }

    @Test
    void testLambdaBootstrapCommandAppendsCustomArguments() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);
        mojo.lambdaBootstrapArguments = List.of("-Dio.netty.noUnsafe=true", "-Dcustom.message=hello world");

        var command = mojo.getLambdaBootstrapCommand();

        assertEquals("./func -XX:MaximumHeapSizePercent=80 -Dio.netty.allocator.numDirectArenas=0 -Dio.netty.noPreferDirect=true -Djava.library.path=$(pwd) -Dio.netty.noUnsafe=true '-Dcustom.message=hello world'", command);
    }

    @Test
    void testBuildDockerfileLeavesDefaultNativeDockerfileUntouched(@TempDir Path tempDir) throws Exception {
        Path dockerfile = tempDir.resolve("DockerfileNative");
        Files.writeString(dockerfile, "FROM builder\nENTRYPOINT [\"/app/application\"]\n");

        Fixtures fixtures = invokeBuildDockerfile(tempDir, dockerfile, DockerfileMojo.DOCKERFILE_NATIVE, true);

        String dockerfileContents = Files.readString(dockerfile);
        assertFalse(dockerfileContents.contains(AbstractDockerMojo.ORACLE_CLOUD_FUNCTION_DEFAULT_CMD));
        verify(fixtures.dockerService).buildImage(fixtures.buildImageCmd);
    }

    @Test
    void testBuildDockerfileAppendsOracleFunctionCmdForBundledOracleDockerfile(@TempDir Path tempDir) throws Exception {
        Path dockerfile = tempDir.resolve("DockerfileNativeOracleCloud");
        Files.writeString(dockerfile, "FROM builder\nENTRYPOINT [\"./func\"]\n");

        Fixtures fixtures = invokeBuildDockerfile(tempDir, dockerfile, DockerfileMojo.DOCKERFILE_NATIVE_ORACLE_CLOUD, false);

        String dockerfileContents = Files.readString(dockerfile);
        assertTrue(dockerfileContents.contains(AbstractDockerMojo.ORACLE_CLOUD_FUNCTION_DEFAULT_CMD));
        verify(fixtures.dockerService).buildImage(fixtures.buildImageCmd);
    }

    @Test
    void testBuildDockerfileCopiesProvidedDockerfileSymlinkContents(@TempDir Path tempDir) throws Exception {
        Assumptions.assumeTrue(supportsSymbolicLinks(tempDir), "Symbolic links are not supported in this test environment");

        Path linkedDockerfileTarget = tempDir.resolve("Dockerfile-custom");
        Files.writeString(linkedDockerfileTarget, "FROM builder\nENTRYPOINT [\"/custom/application\"]\n");
        Path providedDockerfile = tempDir.resolve(DockerfileMojo.DOCKERFILE);
        Files.createSymbolicLink(providedDockerfile, linkedDockerfileTarget.getFileName());

        Fixtures fixtures = new Fixtures(tempDir, tempDir.resolve("DockerfileNative"));
        invokeBuildDockerfile(fixtures.mojo, DockerfileMojo.DOCKERFILE_NATIVE, true);

        Path copiedDockerfile = tempDir.resolve("target").resolve(DockerfileMojo.DOCKERFILE);
        assertTrue(Files.isRegularFile(copiedDockerfile));
        assertFalse(Files.isSymbolicLink(copiedDockerfile));
        assertEquals(Files.readString(linkedDockerfileTarget), Files.readString(copiedDockerfile));
        verify(fixtures.dockerService).buildImage(fixtures.buildImageCmd);
    }

    private boolean supportsSymbolicLinks(Path tempDir) throws IOException {
        Path probeTarget = tempDir.resolve("symlink-probe-target");
        Path probeLink = tempDir.resolve("symlink-probe-link");
        Files.writeString(probeTarget, "probe");
        try {
            Files.createSymbolicLink(probeLink, probeTarget.getFileName());
            return Files.isSymbolicLink(probeLink);
        } catch (IOException | UnsupportedOperationException | SecurityException e) {
            return false;
        } finally {
            Files.deleteIfExists(probeLink);
            Files.deleteIfExists(probeTarget);
        }
    }

    private Fixtures invokeBuildDockerfile(Path tempDir, Path dockerfile, String dockerfileName, boolean passClassName) throws Exception {
        Fixtures fixtures = new Fixtures(tempDir, dockerfile);
        invokeBuildDockerfile(fixtures.mojo, dockerfileName, passClassName);
        return fixtures;
    }

    private void invokeBuildDockerfile(DockerNativeMojo mojo, String dockerfileName, boolean passClassName) throws Exception {
        Method buildDockerfile = DockerNativeMojo.class.getDeclaredMethod("buildDockerfile", String.class, boolean.class);
        buildDockerfile.setAccessible(true);
        try {
            buildDockerfile.invoke(mojo, dockerfileName, passClassName);
        } catch (InvocationTargetException e) {
            throw unwrapInvocationTargetException(e);
        }
    }

    private Exception unwrapInvocationTargetException(InvocationTargetException e) throws Exception {
        Throwable cause = e.getCause();
        if (cause instanceof Exception exception) {
            return exception;
        }
        throw e;
    }

    private static final class Fixtures {
        private final DockerService dockerService;
        private final BuildImageCmd buildImageCmd;
        private final DockerNativeMojo mojo;

        private Fixtures(Path tempDir, Path dockerfile) throws IOException {
            MavenProject project = mock(MavenProject.class);
            MavenSession session = mock(MavenSession.class);
            MojoExecution execution = mock(MojoExecution.class);
            JibConfigurationService jibConfigurationService = mock(JibConfigurationService.class);
            ApplicationConfigurationService applicationConfigurationService = mock(ApplicationConfigurationService.class);
            dockerService = mock(DockerService.class);
            buildImageCmd = mock(BuildImageCmd.class, RETURNS_SELF);

            Build build = new Build();
            build.setDirectory(tempDir.resolve("target").toString());

            Path argsFile = tempDir.resolve("target").resolve("native-image.args");
            Files.createDirectories(argsFile.getParent());
            Files.writeString(argsFile, "--no-fallback\n");

            Properties properties = new Properties();
            properties.setProperty(DockerNativeMojo.ARGS_FILE_PROPERTY_NAME, argsFile.toString());

            when(session.getCurrentProject()).thenReturn(project);
            when(session.getUserProperties()).thenReturn(new Properties());
            when(session.getSystemProperties()).thenReturn(new Properties());
            when(project.getProperties()).thenReturn(properties);
            when(project.getArtifactId()).thenReturn("demo");
            when(project.getBuild()).thenReturn(build);
            when(project.getBasedir()).thenReturn(tempDir.toFile());
            when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
            when(jibConfigurationService.getToImage()).thenReturn(Optional.empty());
            when(jibConfigurationService.getTags()).thenReturn(java.util.Set.of());
            when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));
            when(applicationConfigurationService.getServerPort()).thenReturn("8080");
            when(dockerService.loadDockerfileAsResource(dockerfileNameFor(dockerfile))).thenReturn(dockerfile.toFile());
            when(dockerService.buildImageCmd()).thenReturn(buildImageCmd);
            when(dockerService.buildImage(buildImageCmd)).thenReturn("image-id");

            mojo = new DockerNativeMojo(project, jibConfigurationService, applicationConfigurationService, dockerService, session, execution);
            mojo.baseImageRun = AbstractDockerMojo.DEFAULT_BASE_IMAGE_GRAALVM_RUN;
            mojo.mainClass = "example.Application";
            mojo.staticNativeImage = false;
            mojo.oracleLinuxVersion = "ol9";
            mojo.nativeImageBuildArgs = List.of("--no-fallback");
        }

        private static String dockerfileNameFor(Path dockerfile) {
            return dockerfile.getFileName().toString();
        }
    }

}
