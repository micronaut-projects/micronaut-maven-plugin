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
package io.micronaut.maven.classpath;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;

/**
 * <p>Gives the dependency artifacts their original files back after {@code transform-class-path} and Shade, so that
 * the goals that run later, such as Failsafe's, use the original JARs.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly: the {@code jar} packaging binds
 * it after {@code shade:shade}.</p>
 *
 * @since 5.1.0
 */
@Mojo(name = RestoreClassPathMojo.NAME, defaultPhase = LifecyclePhase.PACKAGE)
public class RestoreClassPathMojo extends AbstractMojo {

    public static final String NAME = "restore-class-path";

    private final MavenProject mavenProject;

    @Inject
    public RestoreClassPathMojo(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
    }

    @Override
    public void execute() {
        if (mavenProject.getContextValue(TransformClassPathMojo.CONTEXT_KEY) instanceof RewrittenClassPath rewritten) {
            rewritten.close();
            mavenProject.setContextValue(TransformClassPathMojo.CONTEXT_KEY, null);
        }
    }
}
