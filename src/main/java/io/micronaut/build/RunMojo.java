package io.micronaut.build;

import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectDependenciesResolver;
import org.apache.maven.toolchain.ToolchainManager;

import javax.inject.Inject;

/**
 * <p>Executes a Micronaut application in development mode.</p>
 *
 * <p>It watches for changes in the project tree. If there are changes in the {@code pom.xml} file, dependencies will be reloaded. If
 * the changes are anywhere underneath {@code src/main}, it will recompile the project and restart the application.</p>
 *
 * <p>The plugin can handle changes in all the languages supported by Micronaut: Java, Kotlin and Groovy.</p>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Mojo(name = "run", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
@Execute(phase = LifecyclePhase.GENERATE_SOURCES, goal = "aot-analysis")
public class RunMojo extends AbstractRunMojo {

    @Inject
    public RunMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager, ProjectDependenciesResolver resolver, ProjectBuilder projectBuilder, ToolchainManager toolchainManager, CompilerService compilerService, ExecutorService executorService) {
        super(mavenProject, mavenSession, pluginManager, resolver, projectBuilder, toolchainManager, compilerService, executorService);
        addClasses = true;
    }
}
