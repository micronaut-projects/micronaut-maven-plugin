/*
 * Copyright 2017-2023 original authors
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
package io.micronaut.maven.enforcer;

import org.apache.maven.enforcer.rule.api.AbstractEnforcerRule;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.XmlUtil;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;

/**
 * Enforcer rule to check that, when the application has YAML configuration, the SnakeYAML library is present.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 4.0.0
 */
@Named("checkSnakeYaml")
public class CheckSnakeYaml extends AbstractEnforcerRule {

    private final MavenProject project;
    private final Dependency snakeYaml;

    @Inject
    public CheckSnakeYaml(MavenProject project) {
        this.project = project;
        this.snakeYaml = new Dependency();
        snakeYaml.setGroupId("org.yaml");
        snakeYaml.setArtifactId("snakeyaml");
        snakeYaml.setScope(JavaScopes.RUNTIME);
    }

    @Override
    public void execute() throws EnforcerRuleException {
        if (hasYamlConfiguration() && !hasSnakeYaml()) {
            String message = "YAML configuration file detected, but SnakeYAML is not on the runtime classpath. Make sure to add the following dependency:" +
                System.lineSeparator();

            message += toXml(snakeYaml);
            throw new EnforcerRuleException(message);
        }
    }

    private boolean hasYamlConfiguration() {
        return this.project.getResources().stream().anyMatch(this::hasYamlConfiguration);
    }

    private boolean hasSnakeYaml() {
        return project.getDependencies().stream()
            .anyMatch(d -> d.getGroupId().equals(snakeYaml.getGroupId()) && d.getArtifactId().equals(snakeYaml.getArtifactId()));
    }

    private boolean hasYamlConfiguration(Resource resource) {
        File[] files = new File(resource.getDirectory()).listFiles();
        if (files != null) {
            return Arrays.stream(files).anyMatch(this::isYamlConfigurationFile);
        } else {
            return false;
        }
    }

    private boolean isYamlConfigurationFile(File f) {
        String name = f.getName().toLowerCase();
        return name.startsWith("application") && (name.endsWith(".yml") || name.endsWith(".yaml"));
    }

    private String toXml(Dependency dependency) {
        StringReader dependencyXml = new StringReader("<dependency>" +
            "<groupId>" + dependency.getGroupId() + "</groupId>" +
            "<artifactId>" + dependency.getArtifactId() + "</artifactId>" +
            "<scope>" + dependency.getScope() + "</scope>" +
            "</dependency>");
        StringWriter result = new StringWriter();
        try {
            XmlUtil.prettyFormat(dependencyXml, result);
            return result.toString();
        } catch (IOException e) {
            return dependencyXml.toString();
        }
    }

    @Override
    public String toString() {
        return "CheckSnakeYaml";
    }
}
