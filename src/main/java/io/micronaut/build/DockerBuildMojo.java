package io.micronaut.build;

import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.jkube.kit.common.AssemblyConfiguration;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;
import org.eclipse.jkube.kit.config.image.build.Arguments;
import org.eclipse.jkube.kit.config.image.build.BuildConfiguration;
import org.eclipse.jkube.kit.config.image.build.DockerFileBuilder;
import org.eclipse.jkube.maven.plugin.mojo.build.BuildMojo;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
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


    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerBuildMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager, CompilerService compilerService, ExecutorService executorService) {
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.compilerService = compilerService;
        this.executorService = executorService;
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.maven.plugins.dependency", "warn");
    }

    @Override
    public void executeInternal() throws MojoExecutionException {
        if (compilerService.needsCompilation()) {
            compilerService.compileProject(false);
        }

        executorService.executeGoal("org.apache.maven.plugins", "maven-dependency-plugin", "3.1.2", "copy-dependencies", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/libs")
        ));

        executorService.executeGoal("org.apache.maven.plugins", "maven-dependency-plugin", "3.1.2", "copy-dependencies", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/libs")
        ));
        executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, "copy-resources", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/resources"),
                element(name("resources"),
                    element(name("resource"),
                        element(name("directory"), "${basedir}/src/main/resources")
                    )
                )
        ));
        executorService.executeGoal("org.apache.maven.plugins:maven-jar-plugin", "jar", configuration(
                element(name("outputDirectory"), "${project.build.directory}/layers/"),
                element(name("archive"),
                        element("addMavenDescriptor", "false"),
                        element(name("manifest"),
                            element("addClasspath", "true"),
                            element("classpathPrefix", "libs/"),
                            element("mainClass", mainClass)
                        ),
                        element("manifestEntries",
                                element("Class-Path", "resources/")
                        )
                )
        ));
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

        Optional<String> configuredFrom = imageConfiguration.map(ic -> ic.getBuildConfiguration().getFrom());
        List<String> portsToExpose = imageConfiguration
                .map(ic -> ic.getBuildConfiguration().getPorts())
                .filter(ports -> !ports.isEmpty())
                .orElse(Collections.singletonList("8080"));

        BuildConfiguration.BuildConfigurationBuilder builder = imageConfiguration
                .map(ImageConfiguration::getBuildConfiguration)
                .map(BuildConfiguration::toBuilder)
                .orElse(BuildConfiguration.builder());

        DockerFileBuilder dfb = new DockerFileBuilder();
        builder.optimise(true);

        switch (micronautRuntime.getBuildStrategy()) {
            case ORACLE_FUNCTION:
                String workdir = "/function";
                String command = "io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest";

                builder.from(configuredFrom.orElse("fnproject/fn-java-fdk:" + determineProjectFnVersion()))
                        .workdir(workdir)
                        .cmd(Arguments.builder().execArgument(command).build());
                break;
            default:
                Arguments.ArgumentsBuilder argBuilder = Arguments.builder()
                        .execArgument("java");
                if (args != null && args.size() > 0) {
                    for (String arg : args) {
                        argBuilder.execArgument(arg);
                    }
                }
                builder.from(configuredFrom.orElse("openjdk:14-alpine"))
                        .workdir("/home/app")
                        .ports(portsToExpose)
                        .entryPoint(argBuilder
                                .execArgument("-jar")
                                .execArgument("/home/app/" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar")
                                .build()
                        ).assembly(
                                AssemblyConfiguration.builder()
                                        .name("target/layers/")
                                        .build()
//                        ).assembly(AssemblyConfiguration.builder()
//                                .targetDir("/")
//                                .inline(Assembly.builder()
//                                        .fileSet(AssemblyFileSet.builder()
//                                                .directory(new File("target/layers/"))
//                                                .outputDirectory(new File("home/app/"))
//                                                .build()
//                                        )
//                                        .build()
//                                ).build()
                        );

                dfb = PluginUtils.createDockerFileBuilder(builder.build(), null);
                dfb.basedir("/");
                dfb.add("target/layers/libs/*.jar", "/home/app/libs");
                dfb.add("target/layers/resources/*", "/home/app/resources");
                dfb.add("target/layers/" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar", "/home/app/application.jar");
                break;
        }

//        AssemblyConfiguration.AssemblyConfigurationBuilder inline = AssemblyConfiguration.builder().name("test").inline(Assembly.builder().baseDirectory(new File(targetDirectory, "layers")).build());

        try {
            File dockerfile = dfb.write(targetDirectory);
            builder.dockerFileFile(dockerfile);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<ImageConfiguration> result = new ArrayList<>();
        String name = imageConfiguration.map(ImageConfiguration::getName).orElse("%g/%a:%l");
        String alias = mavenProject.getName();
        ImageConfiguration ic = new ImageConfiguration(name, alias, null, builder.build(), null, null, null);
        result.add(ic);
        return result;
    }

    private String determineProjectFnVersion() {
        ArtifactVersion javaVersion = new DefaultArtifactVersion(System.getProperty("java.version"));
        if (javaVersion.getMajorVersion() >= 11) {
            return "jre11-latest";
        } else {
            return "latest";
        }
    }


}
