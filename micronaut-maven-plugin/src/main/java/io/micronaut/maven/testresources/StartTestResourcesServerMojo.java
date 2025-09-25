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
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;

import javax.inject.Inject;

/**
 * Starts the Micronaut test resources server.
 */
@Mojo(name = StartTestResourcesServerMojo.NAME, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class StartTestResourcesServerMojo extends AbstractTestResourcesMojo {
    public static final String NAME = "start-testresources-service";

    private final MavenProject mavenProject;

    private final MavenSession mavenSession;

    private final DependencyResolutionService dependencyResolutionService;

    private final ToolchainManager toolchainManager;

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public StartTestResourcesServerMojo(MavenProject mavenProject,
                                        MavenSession mavenSession,
                                        DependencyResolutionService dependencyResolutionService,
                                        ToolchainManager toolchainManager) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
    }

    @Override
    public final void execute() throws MojoExecutionException {
        // Skip starting test resources if tests are being skipped
        if (isTestExecutionSkipped()) {
            getLog().debug("Skipping test resources service start because test execution is disabled");
            return;
        }
        
        var helper = new TestResourcesHelper(testResourcesEnabled, shared, buildDirectory, explicitPort, clientTimeout,
                serverIdleTimeoutMinutes, mavenProject, mavenSession, dependencyResolutionService, toolchainManager,
                testResourcesVersion, classpathInference, testResourcesDependencies, sharedServerNamespace, debugServer,
                foreground, testResourcesSystemProperties);
        helper.start();

    }

    /**
     * Checks whether test execution is skipped using either skipTests or maven.test.skip properties.
     *
     * @return true if tests are being skipped, false otherwise
     */
    private boolean isTestExecutionSkipped() {
        try {
            var execution = new MojoExecution(mavenProject.getPlugin("io.micronaut.maven:micronaut-maven-plugin"), null, null);
            var evaluator = new PluginParameterExpressionEvaluator(mavenSession, execution);
            
            // Check skipTests property (from maven-surefire-plugin)
            Object skipTests = evaluator.evaluate("${skipTests}");
            if (skipTests instanceof Boolean && (Boolean) skipTests) {
                return true;
            }
            if (skipTests instanceof String && Boolean.parseBoolean((String) skipTests)) {
                return true;
            }
            
            // Check maven.test.skip property (skips both compilation and execution)
            Object mavenTestSkip = evaluator.evaluate("${maven.test.skip}");
            if (mavenTestSkip instanceof Boolean && (Boolean) mavenTestSkip) {
                return true;
            }
            if (mavenTestSkip instanceof String && Boolean.parseBoolean((String) mavenTestSkip)) {
                return true;
            }
            
        } catch (ExpressionEvaluationException e) {
            getLog().debug("Could not evaluate test skip properties: " + e.getMessage());
        }
        return false;
    }

}
