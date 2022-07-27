/*
 * Copyright 2017-2021 original authors
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
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Parameter;

import java.io.File;
import java.nio.file.Path;

import static io.micronaut.build.testresources.StopTestResourcesServerMojo.MICRONAUT_TEST_RESOURCES_KEEPALIVE;

/**
 * Base mojo for Micronaut test resources service handling.
 */
public abstract class AbstractTestResourcesMojo extends AbstractMojo {
    public static final String DISABLED = "false";
    public static final String CONFIG_PROPERTY_PREFIX = "micronaut.test.resources.";

    /**
     * Whether to enable or disable Micronaut test resources support.
     */
    @Parameter(property =  CONFIG_PROPERTY_PREFIX + "enabled", defaultValue = DISABLED)
    protected boolean testResourcesEnabled;

    /**
     * Whether the test resources service should be kept alive after the build.
     */
    @Parameter(property = MICRONAUT_TEST_RESOURCES_KEEPALIVE, defaultValue = DISABLED)
    protected Boolean keepAlive = Boolean.valueOf(DISABLED);

    /**
     * Whether the test resources service should be shared between independent builds
     * (e.g different projects, even built with different build tools).
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "shared", defaultValue = DISABLED)
    protected boolean shared;

    @Parameter(defaultValue = "${project.build.directory}", required = true)
    protected File buildDirectory;

    protected final Path getServerSettingsDirectory() {
        if (shared) {
            return ServerUtils.getDefaultSharedSettingsPath();
        }
        return serverSettingsDirectoryOf(buildDirectory.toPath());
    }

    protected final Path getKeepAliveFile() {
        return getServerSettingsDirectory().resolve("keepalive");
    }

    public static Path serverSettingsDirectoryOf(Path buildDir) {
        return buildDir.resolve("../.micronaut/test-resources");
    }

}
