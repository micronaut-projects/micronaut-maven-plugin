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
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * Stops the Micronaut test resources server.
 */
@Mojo(name = MicronautStopTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class MicronautStopTestResourcesServerMojo extends AbstractTestResourcesMojo {
    public static final String NAME = "stop-testresources-service";
    public static final String MICRONAUT_TEST_RESOURCES_KEEPALIVE = "keepAlive";

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (Boolean.TRUE.equals(keepAlive)) {
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
            doExecute();
        } catch (Exception e) {
            throw new MojoExecutionException("Unable to stop test resources server", e);
        }
    }

    private void doExecute() throws IOException {
        getLog().info("Shutting down test resources service");
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

}
