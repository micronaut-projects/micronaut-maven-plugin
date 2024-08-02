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
package io.micronaut.maven.testresources;

import io.micronaut.maven.services.DependencyResolutionService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import javax.inject.Inject;

/**
 * Stops the Micronaut test resources server.
 */
@Mojo(name = StopTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class StopTestResourcesServerMojo extends AbstractTestResourcesMojo {
    public static final String NAME = "stop-testresources-service";
    public static final String MICRONAUT_TEST_RESOURCES_KEEPALIVE = "keepAlive";

    private final MavenProject mavenProject;

    private final MavenSession mavenSession;

    private final DependencyResolutionService dependencyResolutionService;

    private final ToolchainManager toolchainManager;

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public StopTestResourcesServerMojo(MavenProject mavenProject, MavenSession mavenSession,
                                       DependencyResolutionService dependencyResolutionService, ToolchainManager toolchainManager) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
    }

    @Override
    public final void execute() throws MojoExecutionException {
        TestResourcesHelper helper = new TestResourcesHelper(testResourcesEnabled, shared, buildDirectory,
                explicitPort, clientTimeout, serverIdleTimeoutMinutes, mavenProject, mavenSession,
                dependencyResolutionService, toolchainManager, testResourcesVersion,
                classpathInference, testResourcesDependencies, sharedServerNamespace,
                debugServer);
        helper.stop(false);
    }

}
