/*
 * Copyright 2017-2023 original authors
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

import io.micronaut.testresources.buildtools.ServerFactory;
import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.toolchain.ToolchainManager;

import java.io.File;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static io.micronaut.maven.MojoUtils.findJavaExecutable;

/**
 * Default implementation for {@link ServerFactory}.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 4.0.0
 */
public class DefaultServerFactory implements ServerFactory {

    private final Log log;
    private final ToolchainManager toolchainManager;
    private final MavenSession mavenSession;
    private final AtomicBoolean serverStarted;
    private final String testResourcesVersion;
    private final boolean debugServer;
    private final boolean foreground;
    private final Map<String, String> testResourcesSystemProperties;

    private Process process;

    public DefaultServerFactory(Log log,
                                ToolchainManager toolchainManager,
                                MavenSession mavenSession,
                                AtomicBoolean serverStarted,
                                String testResourcesVersion,
                                boolean debugServer,
                                boolean foreground, final Map<String, String> testResourcesSystemProperties) {
        this.log = log;
        this.toolchainManager = toolchainManager;
        this.mavenSession = mavenSession;
        this.serverStarted = serverStarted;
        this.testResourcesVersion = testResourcesVersion;
        this.debugServer = debugServer;
        this.foreground = foreground;
        this.testResourcesSystemProperties = testResourcesSystemProperties;
    }

    @Override
    public void startServer(ServerUtils.ProcessParameters processParameters) {
        log.info("Starting Micronaut Test Resources service, version " + testResourcesVersion);
        var cli = computeCliArguments(processParameters);

        if (log.isDebugEnabled()) {
            log.debug(String.format("Command parameters: %s", String.join(" ", cli)));
        }

        var builder = new ProcessBuilder(cli);
        try {
            process = builder.inheritIO().start();
            if (foreground) {
                log.info("Test Resources Service started in foreground. Press Ctrl+C to stop.");
                process.waitFor();
            }
        } catch (InterruptedException e) {
            log.error("Failed to start server", e);
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            log.error("Failed to start server", e);
            serverStarted.set(false);
            if (process != null) {
                process.destroyForcibly();
            }
        } finally {
            if (process != null) {
                if (process.isAlive()) {
                    serverStarted.set(true);
                } else {
                    process.destroyForcibly();
                }
            }
        }
    }

    /**
     * Computes the command-line arguments required to run the server based on the provided process parameters.
     *
     * @param processParameters the process parameters containing information about JVM arguments, system properties,
     *                          classpath, main class, and program arguments
     * @return a list of command-line arguments as strings
     * @throws IllegalStateException if the Java executable cannot be found, or if the main class is not set
     */
    List<String> computeCliArguments(ServerUtils.ProcessParameters processParameters) {
        var cli = new ArrayList<String>();

        String javaBin = findJavaExecutable(toolchainManager, mavenSession);
        if (javaBin == null) {
            throw new IllegalStateException("Java executable not found");
        }
        cli.add(javaBin);
        cli.addAll(processParameters.getJvmArguments());
        if (debugServer) {
            cli.add("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:8000");
        }
        processParameters.getSystemProperties().forEach((key, value) -> cli.add("-D" + key + "=" + value));
        if (testResourcesSystemProperties != null && !testResourcesSystemProperties.isEmpty()) {
            testResourcesSystemProperties.forEach((key, value) -> cli.add("-D" + key + "=" + value));
        }
        cli.add("-cp");
        cli.add(processParameters.getClasspath().stream()
                .map(File::getAbsolutePath)
                .collect(Collectors.joining(File.pathSeparator)));
        String mainClass = processParameters.getMainClass();
        if (mainClass == null) {
            throw new IllegalStateException("Main class is not set");
        }
        cli.add(mainClass);
        cli.addAll(processParameters.getArguments());

        return cli;
    }

    @Override
    public void waitFor(Duration duration) throws InterruptedException {
        if (process != null) {
            process.waitFor(duration.toMillis(), TimeUnit.MILLISECONDS);
        }
    }
}
