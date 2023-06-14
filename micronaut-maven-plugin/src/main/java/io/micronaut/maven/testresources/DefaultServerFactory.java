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

    private Process process;

    public DefaultServerFactory(Log log, ToolchainManager toolchainManager, MavenSession mavenSession, AtomicBoolean serverStarted) {
        this.log = log;
        this.toolchainManager = toolchainManager;
        this.mavenSession = mavenSession;
        this.serverStarted = serverStarted;
    }

    @Override
    public void startServer(ServerUtils.ProcessParameters processParameters) {
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
        try {
            process = builder.inheritIO().start();
        } catch (Exception e) {
            serverStarted.set(false);
            process.destroyForcibly();
        } finally {
            if (process.isAlive()) {
                serverStarted.set(true);
            } else {
                process.destroyForcibly();
            }
        }
    }

    @Override
    public void waitFor(Duration duration) throws InterruptedException {
        if (process != null) {
            process.waitFor(duration.toMillis(), TimeUnit.MILLISECONDS);
        }
    }
}
