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

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import javax.inject.Inject;
import java.io.File;
import java.util.List;

/**
 * Shared implementation for the AOT sample configuration goal.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 3.2.0
 */
public abstract class AbstractAotSampleMojo extends AbstractMicronautAotCliMojo {

    public static final String SAMPLE_AOT_PROPERTIES_FILE_NAME = "aot.properties";
    public static final String NAME = "aot-sample-config";

    @Inject
    protected AbstractAotSampleMojo(CompilerService compilerService,
                                    ExecutorService executorService,
                                    MavenProject mavenProject,
                                    DependencyResolutionService dependencyResolutionService,
                                    MavenSession mavenSession,
                                    ToolchainManager toolchainManager) {
        super(compilerService, executorService, mavenProject, dependencyResolutionService, mavenSession, toolchainManager);
    }

    @Override
    protected List<String> getExtraArgs() {
        return List.of(
            "--config",
            outputFile(SAMPLE_AOT_PROPERTIES_FILE_NAME).getAbsolutePath()
        );
    }

    @Override
    protected void onSuccess(File outputDir) {
        File sampleFile = new File(outputDir, SAMPLE_AOT_PROPERTIES_FILE_NAME);
        if (sampleFile.exists()) {
            getLog().info("Sample configuration file written to " + sampleFile);
        }
    }

    @Override
    String getName() {
        return NAME;
    }
}
