package io.micronaut.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.codehaus.plexus.util.Os;

import java.io.IOException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.mockito.Mockito.*;

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

    @Test
    void testComputeNativeImageArgsExpandsWindowsStyleNestedArgsFile(@TempDir Path tempDir) throws IOException {
        Path nestedArgsFile = tempDir.resolve("target/tmp/native-image-generated.args");
        Files.createDirectories(nestedArgsFile.getParent());
        Files.write(nestedArgsFile, List.of(
            "\\QC:\\Users\\My User\\.m2\\repository\\com\\example\\demo.jar\\E",
            "-H:ConfigurationFileDirectories=C:\\Users\\My User\\graalvm-reachability-metadata\\metadata\\io.netty\\netty-common\\4.1.80.Final,C:\\Users\\My User\\graalvm-reachability-metadata\\metadata\\io.netty\\netty-buffer\\4.1.80.Final",
            "-H:ConfigurationFileDirectories=C:\\Users\\My User\\workspace\\target\\native\\generated\\generateTestResourceConfig,C:\\Users\\My User\\workspace\\target\\native\\generated\\generateResourceConfig"
        ));

        Path argsFile = tempDir.resolve("graalvm-native-image.args");
        Files.write(argsFile, List.of(
            "--no-fallback",
            "@target\\tmp\\native-image-generated.args"
        ));

        List<String> result = MojoUtils.computeNativeImageArgs(List.of(), "oraclelinux:9", argsFile.toString());

        assertIterableEquals(List.of(
            "--no-fallback",
            "\\Q/home/app/libs/release/demo.jar\\E",
            "-H:ConfigurationFileDirectories=/home/app/graalvm-reachability-metadata/metadata/io.netty/netty-common/4.1.80.Final,/home/app/graalvm-reachability-metadata/metadata/io.netty/netty-buffer/4.1.80.Final",
            "-H:ConfigurationFileDirectories=/home/app/generateTestResourceConfig,/home/app/generateResourceConfig"
        ), result);
    }

    @Test
    void testComputeNativeImageArgsPrefersParentRelativeNestedArgsFile(@TempDir Path tempDir) throws IOException {
        Path nestedArgsFile = tempDir.resolve("target/cwd-collision/native-image-generated.args");
        Files.createDirectories(nestedArgsFile.getParent());
        Files.write(nestedArgsFile, List.of("\\QC:\\parent\\demo.jar\\E"));

        Path cwdCollision = Paths.get("target/cwd-collision/native-image-generated.args");
        Files.createDirectories(cwdCollision.getParent());
        Files.write(cwdCollision, List.of("\\QC:\\cwd\\wrong.jar\\E"));

        Path argsFile = tempDir.resolve("graalvm-native-image.args");
        Files.write(argsFile, List.of("@target\\cwd-collision\\native-image-generated.args"));

        try {
            List<String> result = MojoUtils.computeNativeImageArgs(List.of(), "oraclelinux:9", argsFile.toString());

            assertIterableEquals(List.of("\\Q/home/app/libs/release/demo.jar\\E"), result);
        } finally {
            Files.deleteIfExists(cwdCollision);
            Files.deleteIfExists(cwdCollision.getParent());
        }
    }

    @Test
    void testComputeNativeImageArgsMapsSnapshotDependenciesToSnapshotLayer(@TempDir Path tempDir) throws IOException {
        Path argsFile = tempDir.resolve("graalvm-native-image.args");
        Files.write(argsFile, List.of("\\QC:\\Users\\My User\\.m2\\repository\\com\\example\\demo-1.0-SNAPSHOT.jar\\E"));

        List<String> result = MojoUtils.computeNativeImageArgs(List.of(), "oraclelinux:9", argsFile.toString());

        assertIterableEquals(List.of("\\Q/home/app/libs/snapshot/demo-1.0-SNAPSHOT.jar\\E"), result);
    }
}
