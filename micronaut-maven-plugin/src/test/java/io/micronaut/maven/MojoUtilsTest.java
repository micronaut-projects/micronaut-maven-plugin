package io.micronaut.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.codehaus.plexus.util.Os;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.*;

import java.io.File;

class MojoUtilsTest {

    @Test
    void testFindJavaExecutableWithValidToolchain() {
        // Arrange
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        MavenSession mavenSession = mock(MavenSession.class);
        Toolchain toolchain = mock(Toolchain.class);
        
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(toolchain);
        when(toolchain.findTool("java")).thenReturn("/custom/path/to/java");
        
        // Act
        String result = MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
        
        // Assert
        assertEquals("/custom/path/to/java", result);
    }

    @Test
    void testFindJavaExecutableWithMisconfiguredToolchain() {
        // Arrange
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        MavenSession mavenSession = mock(MavenSession.class);
        Toolchain toolchain = mock(Toolchain.class);
        
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(toolchain);
        when(toolchain.findTool("java")).thenReturn(null); // Simulates misconfigured toolchain
        
        // Act
        String result = MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
        
        // Assert
        assertNotNull(result);
        // Should fallback to default Java executable based on OS
        if (Os.isFamily(Os.FAMILY_UNIX)) {
            assertEquals(new File(new File(System.getProperty("java.home")), "bin/java").getAbsolutePath(), result);
        } else if (Os.isFamily(Os.FAMILY_WINDOWS)) {
            assertEquals(new File(new File(System.getProperty("java.home")), "bin/java.exe").getAbsolutePath(), result);
        } else {
            assertEquals("java", result);
        }
    }

    @Test
    void testFindJavaExecutableWithNoToolchain() {
        // Arrange
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        MavenSession mavenSession = mock(MavenSession.class);
        
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        
        // Act
        String result = MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
        
        // Assert
        assertNotNull(result);
        // Should use default Java executable based on OS
        if (Os.isFamily(Os.FAMILY_UNIX)) {
            assertEquals(new File(new File(System.getProperty("java.home")), "bin/java").getAbsolutePath(), result);
        } else if (Os.isFamily(Os.FAMILY_WINDOWS)) {
            assertEquals(new File(new File(System.getProperty("java.home")), "bin/java.exe").getAbsolutePath(), result);
        } else {
            assertEquals("java", result);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-common/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-buffer/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-handler/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/ch.qos.logback/logback-classic/1.4.1,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-transport/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http2/4.1.80.Final",
            "C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-common\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-buffer\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-handler\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\ch.qos.logback\\logback-classic\\1.4.1,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-transport\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-codec-http\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\someOtherDir\\io.netty\\netty-codec-http2\\4.1.80.Final",
    })
    void testParseReachabilityMetadataConfigFiles(String dirs) {
        String arg = "-H:ConfigurationFileDirectories=%s".formatted(dirs);

        String result = MojoUtils.parseConfigurationFilesDirectoriesArg(arg);

        assertEquals("-H:ConfigurationFileDirectories=/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-common/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-buffer/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-handler/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/ch.qos.logback/logback-classic/1.4.1,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-transport/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http2/4.1.80.Final", result);
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "/someDir/native/generated/generateTestResourceConfig,/someDir/native/generated/generateResourceConfig",
            "C:\\Users\\My User\\someDir\\target\\native\\generated\\generateTestResourceConfig,C:\\Users\\My User\\someDir\\target\\native\\generated\\generateResourceConfig",
    })
    void testParseGenerateResourceConfigFiles(String dirs) {
        String arg = "-H:ConfigurationFileDirectories=%s".formatted(dirs);

        String result = MojoUtils.parseConfigurationFilesDirectoriesArg(arg);

        assertEquals("-H:ConfigurationFileDirectories=/home/app/generateTestResourceConfig,/home/app/generateResourceConfig", result);
    }
}
