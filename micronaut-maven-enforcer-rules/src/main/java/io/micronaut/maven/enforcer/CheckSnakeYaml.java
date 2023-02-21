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
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import javax.inject.Named;

/**
 * TODO: add javadoc.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Named("checkSnakeYaml")
public class CheckSnakeYaml extends AbstractEnforcerRule {

    private final MavenProject project;

    @Inject
    public CheckSnakeYaml(MavenProject project) {
        this.project = project;
    }

    @Override
    public void execute() throws EnforcerRuleException {
        this.getLog().info("Retrieved Target Folder: " + this.project.getBuild().getDirectory());
        this.getLog().info("Retrieved ArtifactId: " + this.project.getArtifactId());
        this.getLog().info("Retrieved Project: " + this.project);
//        this.getLog().info("Retrieved Maven version: " + this.runtimeInformation.getMavenVersion());
//        this.getLog().info("Retrieved Session: " + this.session);
    }

    @Override
    public String toString() {
        return "CheckSnakeYaml";
    }
}
