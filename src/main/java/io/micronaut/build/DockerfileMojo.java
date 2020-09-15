package io.micronaut.build;

import io.fabric8.maven.docker.AbstractDockerMojo;
import io.fabric8.maven.docker.access.ExecException;
import io.fabric8.maven.docker.assembly.DockerFileBuilder;
import io.fabric8.maven.docker.config.Arguments;
import io.fabric8.maven.docker.config.AssemblyConfiguration;
import io.fabric8.maven.docker.config.BuildImageConfiguration;
import io.fabric8.maven.docker.config.ImageConfiguration;
import io.fabric8.maven.docker.service.ServiceHub;
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
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.twdata.maven.mojoexecutor.MojoExecutor.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Mojo(name = "dockerfile", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class DockerfileMojo extends AbstractDockerMojo {

    public static final String MAVEN_SHADE_PLUGIN = "org.apache.maven.plugins:maven-shade-plugin";
    public static final String MAVEN_DEPENDENCY_PLUGIN = "org.apache.maven.plugins:maven-dependency-plugin:3.1.2";

    private final MavenProject mavenProject;
    private final MojoExecutor.ExecutionEnvironment executionEnvironment;

    @Parameter(defaultValue = "${mn.runtime}")
    private MicronautRuntime micronautRuntime;

    @Parameter
    private List<String> args;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    private File targetDirectory;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerfileMojo(MavenProject mavenProject, MavenSession mavenSession, BuildPluginManager pluginManager) {
        this.mavenProject = mavenProject;
        this.executionEnvironment = executionEnvironment(mavenProject, mavenSession, pluginManager);
    }

    @Override
    protected void executeInternal(ServiceHub serviceHub) throws IOException, ExecException, MojoExecutionException {
        if (micronautRuntime == null) {
            micronautRuntime = MicronautRuntime.NONE;
        }

        List<ImageConfiguration> resolvedImages = getResolvedImages();
        Optional<ImageConfiguration> imageConfiguration = Optional.empty();
        if (resolvedImages.size() > 0) {
            if (resolvedImages.size() > 1) {
                if (getLog().isWarnEnabled()) {
                    getLog().warn("Only one image configuration should be defined. The first one will be used");
                }
            }
            imageConfiguration = Optional.of(resolvedImages.get(0));
        }
        Optional<String> configuredFrom = imageConfiguration.map(ic -> ic.getBuildConfiguration().getFrom());
        List<String> portsToExpose = imageConfiguration
                .map(ic -> ic.getBuildConfiguration().getPorts())
                .filter(ports -> !ports.isEmpty())
                .orElse(Collections.singletonList("8080"));

        BuildImageConfiguration.Builder builder = new BuildImageConfiguration.Builder(
                imageConfiguration.map(ImageConfiguration::getBuildConfiguration).orElse(null)
        );

        switch (micronautRuntime) {
            case ORACLE_FUNCTION:
                String workdir = "/function";
                String command = "io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest";

                builder.from(configuredFrom.orElse("fnproject/fn-java-fdk:" + determineProjectFnVersion()))
                        .workdir(workdir)
                        .cmd(Arguments.Builder.get().withParam(command).build());
            break;
            default:
                Arguments.Builder argBuilder = Arguments.Builder.get()
                        .withParam("java");
                if (args != null && args.size() > 0) {
                    for (String arg : args) {
                        argBuilder.withParam(arg);
                    }
                }
                builder.from(configuredFrom.orElse("openjdk:14-alpine"))
                        .workdir("/home/app")
                        .ports(portsToExpose)
                        .entryPoint(argBuilder
                                .withParam("java") //TODO args
                                .withParam("-jar")
                                .withParam("/home/app/application.jar")
                                .build()
                        );

        }

//        PluginUtils.executeGoal(executionEnvironment, mavenProject, "org.apache.maven.plugins", "maven-dependency-plugin", "3.1.2", "copy-dependencies");
//        PluginUtils.executeGoal(executionEnvironment, mavenProject, PluginUtils.MAVEN_RESOURCES_PLUGIN, "copy-resources", configuration(
//                element(name("outputDirectory"), "${basedir}/target/resources"),
//                element(name("resources"),
//                    element(name("resource"),
//                        element(name("directory"), "${basedir}/src/main/resources")
//                    )
//                )
//        ));

        builder.optimise(true);
        DockerFileBuilder dfb = createDockerFileBuilder(builder.build(), null);

        dfb.basedir("/");
        dfb.add("target/dependency/*.jar", "/function/app/");
        dfb.add("target/resources/*", "/function/app/");
        dfb.add("target/original-" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar", "/function/app/");

        targetDirectory.mkdir();
        File result = dfb.write(targetDirectory);
        getLog().info("Dockerfile written to: " + result.getAbsolutePath());
    }

    /*
     * Source: https://github.com/fabric8io/docker-maven-plugin/blob/v0.34.0/src/main/java/io/fabric8/maven/docker/assembly/DockerAssemblyManager.java#L408
     */
    @SuppressWarnings("deprecation")
    DockerFileBuilder createDockerFileBuilder(BuildImageConfiguration buildConfig, AssemblyConfiguration assemblyConfig) {
        DockerFileBuilder builder =
                new DockerFileBuilder()
                        .env(buildConfig.getEnv())
                        .labels(buildConfig.getLabels())
                        .expose(buildConfig.getPorts())
                        .run(buildConfig.getRunCmds())
                        .volumes(buildConfig.getVolumes())
                        .user(buildConfig.getUser());
        if (buildConfig.getMaintainer() != null) {
            builder.maintainer(buildConfig.getMaintainer());
        }
        if (buildConfig.getWorkdir() != null) {
            builder.workdir(buildConfig.getWorkdir());
        }

        builder.exportTargetDir(false);
        builder.baseImage(buildConfig.getFrom());

        if (buildConfig.getHealthCheck() != null) {
            builder.healthCheck(buildConfig.getHealthCheck());
        }

        if (buildConfig.getCmd() != null){
            builder.cmd(buildConfig.getCmd());
        } else if (buildConfig.getCommand() != null) {
            Arguments args = Arguments.Builder.get().withShell(buildConfig.getCommand()).build();
            builder.cmd(args);
        }

        if (buildConfig.getEntryPoint() != null){
            builder.entryPoint(buildConfig.getEntryPoint());
        }

        if (buildConfig.optimise()) {
            builder.optimise();
        }

        return builder;
    }

    private String determineProjectFnVersion() {
        ArtifactVersion javaVersion = new DefaultArtifactVersion(System.getProperty("java.version"));
        if (javaVersion.getMajorVersion() >= 11) {
            return "jre-latest";
        } else {
            return "latest";
        }
    }

}
