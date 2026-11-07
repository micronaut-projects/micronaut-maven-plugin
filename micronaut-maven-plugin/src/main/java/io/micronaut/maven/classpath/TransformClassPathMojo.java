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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * <p>Rewrites the class path that the Shade plugin packages into the fat JAR, with Micronaut AOT's
 * {@code micronaut-aot-bytecode}, when {@code micronaut.classPathTransform.desugarLambdas} or
 * {@code micronaut.classPathTransform.stripLocalVariables} is set. It replaces the application JAR with its
 * rewritten copy and points the dependency artifacts to theirs, until {@code restore-class-path} runs after Shade.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly: the {@code jar} packaging binds
 * it between {@code jar:jar} and {@code shade:shade}.</p>
 *
 * @since 5.1.0
 */
@Mojo(name = TransformClassPathMojo.NAME, defaultPhase = LifecyclePhase.PACKAGE, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class TransformClassPathMojo extends AbstractMojo {

    public static final String NAME = "transform-class-path";

    static final String CONTEXT_KEY = TransformClassPathMojo.class.getName();

    /**
     * Whether the lambdas of the application and of its dependencies are desugared into classes generated at build
     * time, for the Shade fat JAR and the JVM Docker images. It makes starts without a JDK AOT cache faster.
     */
    @Parameter(property = "micronaut.classPathTransform.desugarLambdas", defaultValue = "false")
    private boolean desugarLambdas;

    /**
     * Whether the local-variable tables of the third-party JARs are stripped, for the Shade fat JAR and the JVM
     * Docker images. It makes starts without a JDK AOT cache slightly faster.
     */
    @Parameter(property = "micronaut.classPathTransform.stripLocalVariables", defaultValue = "false")
    private boolean stripLocalVariables;

    /**
     * The version of {@code io.micronaut.aot:micronaut-aot-bytecode} that rewrites the class path.
     */
    @Parameter(property = "micronaut.classPathTransform.version", defaultValue = ClassPathTransformService.DEFAULT_VERSION)
    private String version;

    private final MavenProject mavenProject;
    private final ClassPathTransformService classPathTransformService;

    @Inject
    public TransformClassPathMojo(MavenProject mavenProject, ClassPathTransformService classPathTransformService) {
        this.mavenProject = mavenProject;
        this.classPathTransformService = classPathTransformService;
    }

    @Override
    public void execute() throws MojoExecutionException {
        var options = new ClassPathTransformOptions(desugarLambdas, stripLocalVariables, version);
        if (!options.enabled()) {
            return;
        }
        Artifact artifact = mavenProject.getArtifact();
        File jarFile = artifact.getFile();
        Path application = jarFile != null && jarFile.isFile() ? jarFile.toPath() : Path.of(mavenProject.getBuild().getOutputDirectory());
        RewrittenClassPath rewritten = classPathTransformService.rewrite(mavenProject, options, application, "shade", getLog());
        Path copy = rewritten.packaged(application);
        if (Files.isRegularFile(application) && !copy.equals(application.toAbsolutePath())) {
            try {
                // Shade replaces the project's JAR in place, so the rewritten copy takes its place
                Files.copy(copy, application, StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException e) {
                throw new MojoExecutionException("Unable to replace " + application + " with its rewritten copy", e);
            }
        }
        mavenProject.setContextValue(CONTEXT_KEY, rewritten.swapDependencies(mavenProject));
    }
}
