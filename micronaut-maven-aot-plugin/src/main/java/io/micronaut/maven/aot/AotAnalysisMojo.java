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
package io.micronaut.maven.aot;

import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import javax.inject.Inject;

/**
 * Standalone Micronaut AOT analysis goal.
 */
@Mojo(name = AbstractAotAnalysisMojo.NAME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class AotAnalysisMojo extends AbstractAotAnalysisMojo {

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public AotAnalysisMojo(CompilerService compilerService,
                           ExecutorService executorService,
                           MavenProject mavenProject,
                           DependencyResolutionService dependencyResolutionService,
                           MavenSession mavenSession,
                           ToolchainManager toolchainManager) {
        super(compilerService, executorService, mavenProject, dependencyResolutionService, mavenSession, toolchainManager);
    }

    @Override
    protected boolean shouldExecute() {
        if (MojoUtils.hasMicronautMavenPlugin(mavenProject)) {
            getLog().info("Skipping standalone AOT analysis because micronaut-maven-plugin already owns AOT execution for this build");
            return false;
        }
        return true;
    }

    @Override
    protected boolean alignRuntimeWithPackaging() {
        return false;
    }
}
