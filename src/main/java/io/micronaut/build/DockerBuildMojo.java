package io.micronaut.build;

import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.DockerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;
import org.eclipse.jkube.maven.plugin.mojo.build.BuildMojo;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.util.*;

import static io.micronaut.build.services.CompilerService.MAVEN_RESOURCES_PLUGIN;
import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Mojo(name = "dockerBuild", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class DockerBuildMojo extends BuildMojo {

    @Parameter(defaultValue = "${mn.runtime}")
    private MicronautRuntime micronautRuntime;

    @Parameter
    private List<String> args;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}", required = true)
    private String mainClass;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final MojoExecutor.ExecutionEnvironment executionEnvironment;
    private final CompilerService compilerService;
    private final ExecutorService executorService;
    private final DockerService dockerService;


    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerBuildMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager, CompilerService compilerService, ExecutorService executorService, DockerService dockerService) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.compilerService = compilerService;
        this.executorService = executorService;
        this.dockerService = dockerService;
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
    }

    @Override
    public void executeInternal() throws MojoExecutionException {
        supressDependencyPluginOutput();
        compileProjectIfNeeded();
        copyDependencies();
        copyResources();
        createJar();
        super.executeInternal();
    }

    @Override
    public List<ImageConfiguration> customizeConfig(List<ImageConfiguration> configs) {
        if (micronautRuntime == null) {
            micronautRuntime = MicronautRuntime.NONE;
        }

        Optional<ImageConfiguration> imageConfiguration = Optional.empty();
        if (configs.size() > 0) {
            if (configs.size() > 1) {
                if (getLog().isWarnEnabled()) {
                    getLog().warn("Only one image configuration should be defined. The first one will be used");
                }
            }
            imageConfiguration = Optional.of(configs.get(0));
        }

        List<ImageConfiguration> result = new ArrayList<>();
        result.add(dockerService.createImageConfiguration(imageConfiguration.orElse(null), micronautRuntime, args, targetDirectory));
        return result;
    }

    private void createJar() throws MojoExecutionException {
        String jarMainClass;
        switch (micronautRuntime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                jarMainClass = "com.fnproject.fn.runtime.EntryPoint";
                break;

            case LAMBDA:
                jarMainClass = "io.micronaut.function.aws.runtime.MicronautLambdaRuntime";
                break;

            default:
                jarMainClass = mainClass;
        }

        executorService.executeGoal("org.apache.maven.plugins:maven-jar-plugin", "jar", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/"),
                element(name("archive"),
                        element("addMavenDescriptor", "false"),
                        element(name("manifest"),
                            element("addClasspath", "true"),
                            element("classpathPrefix", "libs/"),
                            element("mainClass", jarMainClass)
                        ),
                        element("manifestEntries",
                                element("Class-Path", "resources/")
                        )
                )
        ));
    }

    private void copyResources() throws MojoExecutionException {
        executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, "copy-resources", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/resources"),
                element(name("resources"),
                    element(name("resource"),
                        element(name("directory"), "${basedir}/src/main/resources")
                    )
                )
        ));
    }

    private void copyDependencies() throws MojoExecutionException {
        executorService.executeGoal("org.apache.maven.plugins", "maven-dependency-plugin", "3.1.2", "copy-dependencies", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/libs")
        ));
    }

    private void compileProjectIfNeeded() {
        if (compilerService.needsCompilation()) {
            compilerService.compileProject(false);
        }
    }

    private void supressDependencyPluginOutput() {
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.maven.plugins.dependency", "warn");
    }

}
