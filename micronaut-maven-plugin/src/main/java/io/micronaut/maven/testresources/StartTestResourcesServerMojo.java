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

import io.micronaut.maven.MojoUtils;
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
     * Checks whether test execution is skipped using either skipTests, maven.test.skip, or skipITs properties.
     *
     * @return true if tests are being skipped, false otherwise
     */
    boolean isTestExecutionSkipped() {
        try {
            var execution = new MojoExecution(mavenProject.getPlugin(MojoUtils.THIS_PLUGIN), null, null);
            var evaluator = new PluginParameterExpressionEvaluator(mavenSession, execution);
            
            // Check skip properties (from maven-surefire-plugin, maven-compiler-plugin, and maven-failsafe-plugin)
            return isPropertyTrue(evaluator, "skipTests") ||
                   isPropertyTrue(evaluator, "maven.test.skip") ||
                   isPropertyTrue(evaluator, "skipITs");
            
        } catch (ExpressionEvaluationException e) {
            getLog().debug("Could not evaluate test skip properties: " + e.getMessage());
        }
        return false;
    }

    /**
     * Checks if a property evaluates to true.
     *
     * @param evaluator the expression evaluator
     * @param propertyName the property name to check
     * @return true if the property is set to true, false otherwise
     * @throws ExpressionEvaluationException if the property cannot be evaluated
     */
    private boolean isPropertyTrue(PluginParameterExpressionEvaluator evaluator, String propertyName) throws ExpressionEvaluationException {
        Object propertyValue = evaluator.evaluate("${" + propertyName + "}");
        if (propertyValue instanceof Boolean) {
            return (Boolean) propertyValue;
        }
        if (propertyValue instanceof String) {
            return Boolean.parseBoolean((String) propertyValue);
        }
        return false;
    }

}
