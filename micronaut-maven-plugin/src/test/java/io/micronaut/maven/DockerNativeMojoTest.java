package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junitpioneer.jupiter.ClearSystemProperty;
import org.junitpioneer.jupiter.RestoreSystemProperties;
import org.junitpioneer.jupiter.SetSystemProperty;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

import static io.micronaut.maven.AbstractDockerMojo.ARM_ARCH;
import static io.micronaut.maven.AbstractDockerMojo.X86_64_ARCH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
@ClearSystemProperty(key = "http.nonProxyHosts")
class DockerNativeMojoTest {

    @ParameterizedTest
    @CsvSource({
            "24,https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.2/graalvm-community-jdk-25.0.2_linux-x64_bin.tar.gz,e0be791c8fda4d03b6b0a0cb824fef3149736170057b3a515252b44419606af0",
            "25,https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.2/graalvm-community-jdk-25.0.2_linux-x64_bin.tar.gz,e0be791c8fda4d03b6b0a0cb824fef3149736170057b3a515252b44419606af0",
            "26,https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.2/graalvm-community-jdk-25.0.2_linux-x64_bin.tar.gz,e0be791c8fda4d03b6b0a0cb824fef3149736170057b3a515252b44419606af0"
    })
    @SetSystemProperty(key = "os.arch", value = X86_64_ARCH)
    void testGraalVmDownloadUrl(String javaVersion, String expectedUrl, String expectedSha256) throws URISyntaxException, IOException, InterruptedException {
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
        var actualSha256 = mojo.graalVmDownloadSha256();

        assertEquals(expectedUrl, actualUrl);
        assertEquals(expectedSha256, actualSha256);

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
        var actualSha256 = mojo.graalVmDownloadSha256();

        var expectedUrl = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.2/graalvm-community-jdk-25.0.2_linux-x64_bin.tar.gz";
        var expectedSha256 = "e0be791c8fda4d03b6b0a0cb824fef3149736170057b3a515252b44419606af0";
        assertEquals(expectedUrl, actualUrl);
        assertEquals(expectedSha256, actualSha256);
    }

    @Test
    @SetSystemProperty(key = "os.arch", value = ARM_ARCH)
    void testGraalVmDownloadUrlForArm() {
        var project = mock(MavenProject.class);
        var session = mock(MavenSession.class);
        var execution = mock(MojoExecution.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(project.getProperties()).thenReturn(new Properties());

        var mojo = new DockerNativeMojo(project, null, null, null, session, execution);

        var actualUrl = mojo.graalVmDownloadUrl();
        var actualSha256 = mojo.graalVmDownloadSha256();

        assertEquals("https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.2/graalvm-community-jdk-25.0.2_linux-aarch64_bin.tar.gz", actualUrl);
        assertEquals("b4580d9f223d0a4b3a1757e58b18ff4c1db950e67e105fc5cb741457d2384a71", actualSha256);
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
    void testLambdaBootstrapCommandAppendsCustomArguments() throws MojoExecutionException {
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
    void escapeClassNameBuildArgEscapesShellExpansionCharacters() throws MojoExecutionException {
        assertEquals("example.Outer\\$Inner", DockerNativeMojo.escapeClassNameBuildArg("example.Outer$Inner"));
    }

    @Test
    void escapeClassNameBuildArgRejectsControlCharacters() {
        assertThrows(MojoExecutionException.class, () -> DockerNativeMojo.escapeClassNameBuildArg("example.App\nRUN echo injected"));
    }

}
