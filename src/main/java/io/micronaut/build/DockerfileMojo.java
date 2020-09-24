package io.micronaut.build;

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
import org.eclipse.jkube.kit.build.service.docker.ServiceHub;
import org.eclipse.jkube.kit.build.service.docker.access.ExecException;
import org.eclipse.jkube.kit.common.AssemblyConfiguration;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;
import org.eclipse.jkube.kit.config.image.ImageConfiguration.ImageConfigurationBuilder;
import org.eclipse.jkube.kit.config.image.build.Arguments;
import org.eclipse.jkube.kit.config.image.build.BuildConfiguration;
import org.eclipse.jkube.kit.config.image.build.BuildConfiguration.BuildConfigurationBuilder;
import org.eclipse.jkube.kit.config.image.build.DockerFileBuilder;
import org.eclipse.jkube.maven.plugin.mojo.build.AbstractDockerMojo;
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
    public List<ImageConfiguration> customizeConfig(List<ImageConfiguration> configs) {
        return configs;
    }

    @Override
    protected void executeInternal() throws IOException, MojoExecutionException {
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

        BuildConfigurationBuilder builder = imageConfiguration
                .map(ImageConfiguration::getBuildConfiguration)
                .map(BuildConfiguration::toBuilder)
                .orElse(BuildConfiguration.builder());

        switch (micronautRuntime) {
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
                                .execArgument("java") //TODO args
                                .execArgument("-jar")
                                .execArgument("/home/app/application.jar")
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
     * Source: https://github.com/eclipse/jkube/blob/v1.0.0/jkube-kit/build/api/src/main/java/org/eclipse/jkube/kit/build/api/assembly/AssemblyManager.java#L247
     */
    @SuppressWarnings("deprecation")
    DockerFileBuilder createDockerFileBuilder(BuildConfiguration buildConfig, AssemblyConfiguration assemblyConfig) {
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
        if (assemblyConfig != null) {
            builder.add(assemblyConfig.getTargetDir(), "")
                    .basedir(assemblyConfig.getTargetDir())
                    .assemblyUser(assemblyConfig.getUser())
                    .exportTargetDir(assemblyConfig.getExportTargetDir());
        } else {
            builder.exportTargetDir(false);
        }

        builder.baseImage(buildConfig.getFrom());

        if (buildConfig.getHealthCheck() != null) {
            builder.healthCheck(buildConfig.getHealthCheck());
        }

        if (buildConfig.getCmd() != null){
            builder.cmd(buildConfig.getCmd());
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
