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
package org.apache.maven.plugins.enforcer;

import org.apache.maven.enforcer.rule.api.EnforcerRule;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.enforcer.rule.api.EnforcerRuleHelper;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.util.xml.XmlUtil;
import org.eclipse.aether.util.artifact.JavaScopes;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;

/**
 * Compatibility fallback for Maven Enforcer's deprecated class-name based rule loading.
 */
public class CheckSnakeYaml implements EnforcerRule {

    @Override
    public void execute(EnforcerRuleHelper helper) throws EnforcerRuleException {
        MavenProject project = project(helper);
        if (hasYamlConfiguration(project) && !hasSnakeYaml(project)) {
            throw new EnforcerRuleException(message());
        }
    }

    @Override
    public boolean isCacheable() {
        return false;
    }

    @Override
    public boolean isResultValid(EnforcerRule cachedRule) {
        return false;
    }

    @Override
    public String getCacheId() {
        return null;
    }

    private MavenProject project(EnforcerRuleHelper helper) throws EnforcerRuleException {
        try {
            return (MavenProject) helper.evaluate("${project}");
        } catch (ExpressionEvaluationException e) {
            throw new EnforcerRuleException("Failed to resolve Maven project", e);
        }
    }

    private boolean hasYamlConfiguration(MavenProject project) {
        return project.getResources().stream().anyMatch(this::hasYamlConfiguration);
    }

    private boolean hasSnakeYaml(MavenProject project) {
        return project.getDependencies().stream()
            .anyMatch(d -> d.getGroupId().equals("org.yaml") && d.getArtifactId().equals("snakeyaml"));
    }

    private boolean hasYamlConfiguration(Resource resource) {
        File[] files = new File(resource.getDirectory()).listFiles();
        if (files == null) {
            return false;
        }
        return Arrays.stream(files).anyMatch(this::isYamlConfigurationFile);
    }

    private boolean isYamlConfigurationFile(File file) {
        String name = file.getName().toLowerCase();
        return name.startsWith("application") && (name.endsWith(".yml") || name.endsWith(".yaml"));
    }

    private String message() {
        Dependency snakeYaml = new Dependency();
        snakeYaml.setGroupId("org.yaml");
        snakeYaml.setArtifactId("snakeyaml");
        snakeYaml.setScope(JavaScopes.RUNTIME);
        StringReader dependencyXml = new StringReader("<dependency>"
            + "<groupId>" + snakeYaml.getGroupId() + "</groupId>"
            + "<artifactId>" + snakeYaml.getArtifactId() + "</artifactId>"
            + "<scope>" + snakeYaml.getScope() + "</scope>"
            + "</dependency>");
        StringWriter result = new StringWriter();
        try {
            XmlUtil.prettyFormat(dependencyXml, result);
            return "YAML configuration file detected, but SnakeYAML is not on the runtime classpath. Make sure to add the following dependency:"
                + System.lineSeparator()
                + result;
        } catch (IOException e) {
            return "YAML configuration file detected, but SnakeYAML is not on the runtime classpath. Make sure to add the following dependency:"
                + System.lineSeparator()
                + dependencyXml;
        }
    }
}
