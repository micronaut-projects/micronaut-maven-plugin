package io.micronaut.build;

import com.github.dockerjava.api.command.BuildImageCmd;
import com.github.dockerjava.api.command.CreateContainerResponse;
import io.micronaut.build.services.JibConfigurationService;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import static io.micronaut.build.AwsCustomRuntimeMojo.AWS_CUSTOM_RUNTIME_PACKAGING;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
@Mojo(name = AWS_CUSTOM_RUNTIME_PACKAGING)
public class AwsCustomRuntimeMojo extends AbstractDockerNativeMojo {

    public static final String AWS_CUSTOM_RUNTIME_PACKAGING = "aws-custom-runtime";

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public AwsCustomRuntimeMojo(MavenProject mavenProject, JibConfigurationService jibConfigurationService) {
        super(mavenProject, jibConfigurationService);
    }

    @Override
    protected void postProcess(String imageId) {
        CreateContainerResponse container = dockerClient.createContainerCmd(imageId).exec();
        dockerClient.startContainerCmd(container.getId());
        InputStream nativeImage = dockerClient.copyArchiveFromContainerCmd(container.getId(), "/function/function.zip").exec();

        try (TarArchiveInputStream fin = new TarArchiveInputStream(nativeImage)) {
            TarArchiveEntry tarEntry = fin.getNextTarEntry();
            File functionZip = new File(targetDirectory, tarEntry.getName());
            IOUtils.copy(fin, new FileOutputStream(functionZip));

            getLog().info("AWS Lambda Custom Runtime ZIP: " + functionZip.getPath());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected String getSupportedPackaging() {
        return AWS_CUSTOM_RUNTIME_PACKAGING;
    }

    @Override
    protected BuildImageCmd buildImageCmd() throws IOException {
        File dockerfile = loadDockerfileAsResource("/dockerfiles/DockerfileAwsCustomRuntime");

        String graalVmJvmVersion = "java8";
        if (javaVersion().getMajorVersion() >= 11) {
            graalVmJvmVersion = "java11";
        }

        //TODO read GraalVM native-image plugin config to look for additional args
        return dockerClient.buildImageCmd(dockerfile)
                .withBuildArg("GRAALVM_VERSION", graalVmVersion())
                .withBuildArg("GRAALVM_JVM_VERSION", graalVmJvmVersion);
    }
}
