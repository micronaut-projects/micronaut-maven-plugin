/*
 * Copyright 2017-2026 original authors
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
package io.micronaut.maven.jsonschema;

import io.micronaut.maven.services.CompilerService;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
import java.nio.file.Path;
import java.util.List;

/**
 * Validates Micronaut configuration for tests.
 */
@Mojo(
    name = ValidateTestConfigurationMojo.MOJO_NAME,
    defaultPhase = LifecyclePhase.TEST,
    requiresDependencyResolution = ResolutionScope.TEST
)
public class ValidateTestConfigurationMojo extends AbstractConfigurationValidationMojo {
    public static final String MOJO_NAME = "validate-test-configuration";

    @Inject
    public ValidateTestConfigurationMojo(MavenProject project, CompilerService compilerService) {
        super(project, compilerService);
    }

    @Override
    protected String scenarioName() {
        return "test";
    }

    @Override
    protected ConfigurationValidationConfiguration.ValidationSet validationSet(ConfigurationValidationConfiguration cfg) {
        return cfg.getTest();
    }

    @Override
    protected List<String> defaultEnvironments() {
        return List.of("test");
    }

    @Override
    protected List<String> defaultClasspath(MavenProject project) {
        return ConfigurationValidationClasspath.defaultTestClasspath(project);
    }

    @Override
    protected String[] dependencyScopes() {
        return new String[] { JavaScopes.TEST };
    }

    @Override
    protected List<Path> defaultResourceDirectories() {
        return List.of(Path.of("src/main/resources"), Path.of("src/test/resources"));
    }
}
