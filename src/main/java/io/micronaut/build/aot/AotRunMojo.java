/*
 * Copyright 2003-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.build.aot;

import io.micronaut.build.AbstractRunMojo;
import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectDependenciesResolver;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.graph.Dependency;

import javax.inject.Inject;
import java.io.File;
import java.util.Comparator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Mojo(name = "aot-run", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PACKAGE)
public class AotRunMojo extends AbstractRunMojo {
    @Parameter(defaultValue = "${project.build.finalName}", readonly = true)
    private String finalName;

    @Inject
    public AotRunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager, ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder, ToolchainManager toolchainManager, CompilerService compilerService, ExecutorService executorService) {
        super(mavenProject, mavenSession, pluginManager, resolver, projectBuilder, toolchainManager, compilerService, executorService);
    }

    @Override
    protected void buildClasspath() {
        Comparator<Dependency> byGroupId = Comparator.comparing(d -> d.getArtifact().getGroupId());
        Comparator<Dependency> byArtifactId = Comparator.comparing(d -> d.getArtifact().getArtifactId());
        File mainArtifact = mavenProject.getArtifact().getFile();
        File aotArtifact = new File(mainArtifact.getParentFile(), finalName + "-aot.jar");
        classpath = Stream.concat(
                        Stream.of(aotArtifact.getAbsolutePath()),
                        projectDependencies.stream()
                                .sorted(byGroupId.thenComparing(byArtifactId))
                                .filter(dependency -> !dependency.getArtifact().getFile().equals(mainArtifact))
                                .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
                ).collect(Collectors.joining(File.pathSeparator));
    }

}
