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
package io.micronaut.build.testresources;

import io.micronaut.testresources.buildtools.ServerUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * Utility class to stop Test Resources service.
 */
public class StopTestResourcesHelper {

    private final boolean enabled;

    private final boolean keepAlive;

    private final boolean shared;

    private final Log log;

    private final File buildDirectory;

    public StopTestResourcesHelper(boolean enabled, boolean keepAlive, boolean shared, Log log, File buildDirectory) {
        this.enabled = enabled;
        this.keepAlive = keepAlive;
        this.shared = shared;
        this.log = log;
        this.buildDirectory = buildDirectory;
    }

    /**
     * Contains the logic to stop the Test Resources Service.
     */
    public void stopTestResources() throws MojoExecutionException {
        if (!enabled || Boolean.TRUE.equals(keepAlive)) {
            return;
        }
        if (Files.exists(getKeepAliveFile())) {
            try {
                Files.delete(getKeepAliveFile());
            } catch (IOException e) {
                throw new MojoExecutionException("Failed to delete keepalive file", e);
            }
            return;
        }
        try {
            doStop();
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to stop test resources server", e);
        }
    }

    private void doStop() throws IOException {
        log.info("Shutting down Micronaut Test Resources service");
        Path buildDir = buildDirectory.toPath();
        ServerUtils.stopServer(buildDir.resolve("test-classes"));
        Files.walkFileTree(getServerSettingsDirectory(), new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return super.visitFile(file, attrs);
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                Files.delete(dir);
                return super.postVisitDirectory(dir, exc);
            }
        });
    }

    private Path getServerSettingsDirectory() {
        if (shared) {
            return ServerUtils.getDefaultSharedSettingsPath();
        }
        return serverSettingsDirectoryOf(buildDirectory.toPath());
    }

    private Path getKeepAliveFile() {
        return getServerSettingsDirectory().resolve("keepalive");
    }

    private Path serverSettingsDirectoryOf(Path buildDir) {
        return buildDir.resolve("../.micronaut/test-resources");
    }
}
