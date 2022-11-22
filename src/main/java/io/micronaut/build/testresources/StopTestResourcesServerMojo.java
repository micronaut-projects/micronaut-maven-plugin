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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Stops the Micronaut test resources server.
 */
@Mojo(name = StopTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class StopTestResourcesServerMojo extends AbstractTestResourcesMojo {
    public static final String NAME = "stop-testresources-service";
    public static final String MICRONAUT_TEST_RESOURCES_KEEPALIVE = "keepAlive";

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        TestResourcesHelper helper = new TestResourcesHelper(testResourcesEnabled, keepAlive, shared, buildDirectory);
        helper.stop();
    }

}
