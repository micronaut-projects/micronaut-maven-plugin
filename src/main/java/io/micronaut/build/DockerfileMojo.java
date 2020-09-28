package io.micronaut.build;

import io.micronaut.build.services.DockerService;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;
import org.eclipse.jkube.kit.config.image.build.DockerFileBuilder;
import org.eclipse.jkube.maven.plugin.mojo.build.AbstractDockerMojo;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.util.List;

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
    private final DockerService dockerService;

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
    public DockerfileMojo(MavenProject mavenProject, DockerService dockerService) {
        this.mavenProject = mavenProject;
        this.dockerService = dockerService;
    }

    @Override
    public List<ImageConfiguration> customizeConfig(List<ImageConfiguration> configs) {
        return configs;
    }

    @Override
    protected void executeInternal() throws IOException {
        if (micronautRuntime == null) {
            micronautRuntime = MicronautRuntime.NONE;
        }

        List<ImageConfiguration> resolvedImages = getResolvedImages();
        ImageConfiguration userDefinedImageConfiguration = new ImageConfiguration();
        if (resolvedImages.size() > 0) {
            if (resolvedImages.size() > 1) {
                if (getLog().isWarnEnabled()) {
                    getLog().warn("Only one image configuration should be defined. The first one will be used");
                }
            }
            userDefinedImageConfiguration = resolvedImages.get(0);
        }

        ImageConfiguration imageConfiguration = dockerService.createImageConfiguration(userDefinedImageConfiguration, micronautRuntime, args, targetDirectory);
        DockerFileBuilder dfb = PluginUtils.createDockerFileBuilder(imageConfiguration.getBuild(), null);
        dfb.basedir("/");

        if (micronautRuntime.getBuildStrategy() == DockerBuildStrategy.ORACLE_FUNCTION) {
            dfb.add("target/layers/libs/*.jar", "/function/app");
            dfb.add("target/layers/resources/*", "/function/app");
            dfb.add("target/layers/" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar", "/function/app/application.jar");
        } else {
            dfb.add("target/layers/libs/*.jar", "/home/app/libs");
            dfb.add("target/layers/resources/*", "/home/app/resources");
            dfb.add("target/layers/" + mavenProject.getArtifactId() + "-" + mavenProject.getVersion() + ".jar", "/home/app/application.jar");
        }

        targetDirectory.mkdirs();
        File result = dfb.write(targetDirectory);
        getLog().info("Dockerfile written to: " + result.getAbsolutePath());
    }


}
