package io.micronaut.build.services;

import io.micronaut.build.MicronautRuntime;
import io.micronaut.build.PluginUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.project.MavenProject;
import org.eclipse.jkube.kit.common.Assembly;
import org.eclipse.jkube.kit.common.AssemblyConfiguration;
import org.eclipse.jkube.kit.common.AssemblyFileSet;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;
import org.eclipse.jkube.kit.config.image.build.Arguments;
import org.eclipse.jkube.kit.config.image.build.BuildConfiguration;
import org.eclipse.jkube.kit.config.image.build.DockerFileBuilder;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Singleton
public class DockerService {

    private final MavenProject mavenProject;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public DockerService(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
    }

    public ImageConfiguration createImageConfiguration(ImageConfiguration userDefinedImageConfiguration, MicronautRuntime micronautRuntime, List<String> args, File targetDirectory) {
        Optional<ImageConfiguration> imageConfiguration = Optional.ofNullable(userDefinedImageConfiguration);

        Optional<String> configuredFrom = imageConfiguration
                .map(ImageConfiguration::getBuildConfiguration)
                .map(BuildConfiguration::getFrom);

        List<String> portsToExpose = imageConfiguration
                .map(ImageConfiguration::getBuildConfiguration)
                .map(BuildConfiguration::getPorts)
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
                        ).assembly(AssemblyConfiguration.builder()
                                .name("target")
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


                BuildConfiguration buildConfiguration = builder.build();
                dfb = PluginUtils.createDockerFileBuilder(buildConfiguration, buildConfiguration.getAssembly());
                dfb.basedir("/");
                dfb.add("target/layers/libs/*.jar", "/home/app/libs");
                dfb.add("target/layers/resources/*", "/home/app/resources");
                dfb.add("target/layers/" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar", "/home/app/application.jar");
                break;
        }

        try {
            File dockerfile = dfb.write(targetDirectory);
            builder = BuildConfiguration.builder();
            builder.dockerFile(dockerfile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }

        String name = imageConfiguration.map(ImageConfiguration::getName).orElse("%g/%a:%l");
        String alias = mavenProject.getName();
        return new ImageConfiguration(name, alias, null, builder.build(), null, null, null);

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
