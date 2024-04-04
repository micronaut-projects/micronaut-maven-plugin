package io.micronaut.maven;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MojoUtilsTest {

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
