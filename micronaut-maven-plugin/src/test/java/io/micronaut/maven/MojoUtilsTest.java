package io.micronaut.maven;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MojoUtilsTest {

    @Test
    void testParseReachabilityMetadataConfigFiles() {
        String arg = "-H:ConfigurationFileDirectories=/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-common/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-buffer/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-handler/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/ch.qos.logback/logback-classic/1.4.1,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-transport/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http/4.1.80.Final,/someDir/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http2/4.1.80.Final";

        String result = MojoUtils.parseConfigurationFilesDirectoriesArg(arg);

        assertEquals("-H:ConfigurationFileDirectories=/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-common/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-buffer/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-handler/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/ch.qos.logback/logback-classic/1.4.1,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-transport/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http/4.1.80.Final,/home/app/graalvm-reachability-metadata/someOtherDir/io.netty/netty-codec-http2/4.1.80.Final", result);
    }

    @Test
    void testParseGenerateResourceConfigFiles() {
        String arg = "-H:ConfigurationFileDirectories=/someDir/native/generated/generateTestResourceConfig,/someDir/native/generated/generateResourceConfig";

        String result = MojoUtils.parseConfigurationFilesDirectoriesArg(arg);

        assertEquals("-H:ConfigurationFileDirectories=/home/app/generateTestResourceConfig,/home/app/generateResourceConfig", result);
    }
}
