package io.micronaut.build.jib;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.BuildImageResultCallback;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.model.BuildResponseItem;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;
import com.github.dockerjava.zerodep.ZerodepDockerHttpClient;
import com.google.cloud.tools.jib.api.buildplan.*;
import com.google.cloud.tools.jib.buildplan.UnixPathParser;
import com.google.cloud.tools.jib.maven.extension.JibMavenPluginExtension;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.maven.extension.nativeimage.JibNativeImageExtension;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import io.micronaut.build.MicronautRuntime;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.*;

import static com.google.cloud.tools.jib.plugins.extension.ExtensionLogger.LogLevel.ERROR;
import static com.google.cloud.tools.jib.plugins.extension.ExtensionLogger.LogLevel.LIFECYCLE;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class JibMicronautExtension implements JibMavenPluginExtension<Void> {

    private final JibNativeImageExtension nativeImageExtension;
    private final DockerClient dockerClient;

    public JibMicronautExtension() {
        nativeImageExtension = new JibNativeImageExtension();
        DockerClientConfig config = DefaultDockerClientConfig.createDefaultConfigBuilder().build();
        DockerHttpClient httpClient = new ZerodepDockerHttpClient.Builder()
                .dockerHost(config.getDockerHost())
                .sslConfig(config.getSSLConfig())
                .build();
        dockerClient = DockerClientImpl.getInstance(config, httpClient);
    }

    @Override
    public Optional<Class<Void>> getExtraConfigType() {
        return Optional.empty();
    }

    @Override
    public ContainerBuildPlan extendContainerBuildPlan(ContainerBuildPlan buildPlan, Map<String, String> properties,
                                                       Optional<Void> extraConfig, MavenData mavenData,
                                                       ExtensionLogger logger) throws JibPluginExtensionException {

        ContainerBuildPlan.Builder builder = buildPlan.toBuilder();
        String packaging = mavenData.getMavenProject().getPackaging();
        if ("docker-native".equals(packaging)) {
            logger.log(LIFECYCLE, "Will generate a Native Image inside a Docker image");
            try {
                copyDependencies(mavenData.getMavenProject());

                String targetDir = mavenData.getMavenProject().getBuild().getDirectory();
                File dockerfile = new File(targetDir, "Dockerfile");
                //TODO publish image
                FileUtils.write(dockerfile, "FROM alvarosanchez/micronaut-native-image-builder:20.2.0-java11", Charset.defaultCharset());

                BuildImageResultCallback resultCallback = new BuildImageResultCallback() {
                    @Override
                    public void onNext(BuildResponseItem item) {
                        super.onNext(item);

                        if (item.isErrorIndicated()) {
                            logger.log(ERROR, item.getErrorDetail().getMessage());
                        } else {
                            logger.log(LIFECYCLE, StringUtils.chomp(item.getStream(), "\n"));
                        }
                    }
                };

                String imageId = dockerClient.buildImageCmd(dockerfile)
                        .withBuildArg("className", Objects.requireNonNull(buildPlan.getEntrypoint()).get(3))
                        .exec(resultCallback)
                        .awaitImageId();

                CreateContainerResponse container = dockerClient.createContainerCmd(imageId).exec();
                dockerClient.startContainerCmd(container.getId());
                InputStream nativeImage = dockerClient.copyArchiveFromContainerCmd(container.getId(), "/home/app/application").exec();

                try (TarArchiveInputStream fin = new TarArchiveInputStream(nativeImage)) {
                    TarArchiveEntry tarEntry = fin.getNextTarEntry();
                    IOUtils.copy(fin, new FileOutputStream(new File(targetDir, tarEntry.getName())));
                }

                //TODO publish base image
                builder.setBaseImage("alvarosanchez/micronaut-docker-native:latest")
                        .setLayers(Collections.emptyList());

                properties.putIfAbsent("imageName", "application");
                return nativeImageExtension.extendContainerBuildPlan(builder.build(), properties, Optional.empty(), mavenData, logger);
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            //TODO make a best-effort guess
            MicronautRuntime runtime = MicronautRuntime.valueOf(properties.getOrDefault("micronautRuntime", "NONE"));

            //TODO be able to tell if the user has configured a from - read plugin configuration using Maven API?
            builder.setBaseImage("openjdk:14-alpine")
                    //TODO detect ports
                    .addExposedPort(Port.tcp(8080));

            switch (runtime.getBuildStrategy()) {
                case ORACLE_FUNCTION:

                    List<? extends LayerObject> originalLayers = buildPlan.getLayers();
                    builder.setLayers(Collections.emptyList());

                    for (LayerObject layer : originalLayers) {
                        builder.addLayer(remapLayer(layer));
                    }

                    builder.setBaseImage("fnproject/fn-java-fdk:" + determineProjectFnVersion())
                            .setWorkingDirectory(AbsoluteUnixPath.get("/function"))
                            .setEntrypoint(null)
                            .setCmd(Collections.singletonList("io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"));
                    break;
                case LAMBDA:
                    List<String> entrypoint = buildPlan.getEntrypoint();
                    Objects.requireNonNull(entrypoint).set(entrypoint.size() - 1, "io.micronaut.function.aws.runtime.MicronautLambdaRuntime");
                    builder.setEntrypoint(entrypoint);
                    break;
            }
        }
        return builder.build();
    }

    private LayerObject remapLayer(LayerObject layerObject) {
        FileEntriesLayer originalLayer = (FileEntriesLayer) layerObject;
        FileEntriesLayer.Builder builder = FileEntriesLayer.builder().setName(originalLayer.getName());
        for (FileEntry originalEntry : originalLayer.getEntries()) {
            builder.addEntry(remapEntry(originalEntry));
        }

        return builder.build();
    }

    private FileEntry remapEntry(FileEntry originalEntry) {
        List<String> pathComponents = UnixPathParser.parse(originalEntry.getExtractionPath().toString());

        AbsoluteUnixPath newPath = AbsoluteUnixPath.get("/function/app/" + pathComponents.get(pathComponents.size() - 1));
        return new FileEntry(originalEntry.getSourceFile(), newPath, originalEntry.getPermissions(),
                originalEntry.getModificationTime(), originalEntry.getOwnership());
    }

    private String determineProjectFnVersion() {
        ArtifactVersion javaVersion = new DefaultArtifactVersion(System.getProperty("java.version"));
        if (javaVersion.getMajorVersion() >= 11) {
            return "jre11-latest";
        } else {
            return "latest";
        }
    }

    private void copyDependencies(MavenProject project) throws IOException {
        List<String> imageClasspathScopes = Arrays.asList(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);
        project.setArtifactFilter(artifact -> imageClasspathScopes.contains(artifact.getScope()));
        File target = new File(project.getBuild().getDirectory(), "dependency");
        target.mkdirs();
        for (Artifact dependency : project.getArtifacts()) {
            Files.copy(dependency.getFile().toPath(), target.toPath().resolve(dependency.getFile().getName()));
        }
    }


}
