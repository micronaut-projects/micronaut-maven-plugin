package io.micronaut.build;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public abstract class AbstractDockerNativeMojo extends AbstractMojo {

    public static final String DEFAULT_GRAAL_VERSION = "20.2.0";
    public static final String DEFAULT_GRAAL_JVM_VERSION = "java11";

    protected final MavenProject mavenProject;
    protected final JibConfigurationService jibConfigurationService;
    protected final DockerClient dockerClient;

    /**
     * The project's target directory.
     */
    @Parameter(defaultValue = "${project.build.directory}")
    protected File targetDirectory;

    /**
     * The main class of the application, as defined in the
     * <a href="https://www.mojohaus.org/exec-maven-plugin/java-mojo.html#mainClass">Exec Maven Plugin</a>.
     */
    @Parameter(defaultValue = "${exec.mainClass}", required = true)
    protected String mainClass;


    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public AbstractDockerNativeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        this.mavenProject = mavenProject;
        this.jibConfigurationService = jibConfigurationService;
        DockerClientConfig config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
        DockerHttpClient httpClient = new ZerodepDockerHttpClient.Builder()
                .dockerHost(config.getDockerHost())
                .sslConfig(config.getSSLConfig())
                .build();
        dockerClient = DockerClientImpl.getInstance(config, httpClient);
    }

    @Override
    public void execute() {
        String packaging = mavenProject.getPackaging();

        if (getSupportedPackaging().equals(packaging)) {
            try {
                copyDependencies();

                BuildImageResultCallback resultCallback = new BuildImageResultCallback() {
                    @Override
                    public void onNext(BuildResponseItem item) {
                        super.onNext(item);
                        if (item.isErrorIndicated()) {
                            getLog().error(item.getErrorDetail().getMessage());
                        } else {
                            getLog().info(StringUtils.chomp(item.getStream(), "\n"));
                        }
                    }
                };

                //TODO read GraalVM native-image plugin config to look for additional args
                String imageId = buildImageCmd()
                        .exec(resultCallback)
                        .awaitImageId();

                postProcess(imageId);
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            getLog().warn("Skipping. POM packaging should be set to " + getSupportedPackaging());
        }

    }

    protected void postProcess(String imageId) {

    }

    protected abstract String getSupportedPackaging();

    protected abstract BuildImageCmd buildImageCmd() throws IOException;

    protected File loadDockerfileAsResource(String path) throws IOException {
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) {
            String fileName = path.substring(path.lastIndexOf("/") + 1);
            File dockerfile = new File(targetDirectory, fileName);
            FileUtils.copyInputStreamToFile(stream, dockerfile);
            return dockerfile;
        }
        return null;
    }

    protected String graalVmVersion() {
        return mavenProject.getProperties().getProperty("graal.version", DEFAULT_GRAAL_VERSION);
    }

    protected ArtifactVersion javaVersion() {
        return new DefaultArtifactVersion(Optional.ofNullable(mavenProject.getProperties().getProperty("maven.compiler.target")).orElse(System.getProperty("java.version")));
    }

    private void copyDependencies() throws IOException {
        List<String> imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        mavenProject.setArtifactFilter(artifact -> imageClasspathScopes.contains(artifact.getScope()));
        File target = new File(mavenProject.getBuild().getDirectory(), "dependency");
        if (!target.exists()) {
            target.mkdirs();
        }
        for (Artifact dependency : mavenProject.getArtifacts()) {
            copyDependency(dependency.getFile());
        }
    }

    protected void copyDependency(File dependency) throws IOException {
        File target = new File(mavenProject.getBuild().getDirectory(), "dependency");
        if (!target.exists()) {
            target.mkdirs();
        }
        Files.copy(dependency.toPath(), target.toPath().resolve(dependency.getName()), StandardCopyOption.REPLACE_EXISTING);
    }
}
