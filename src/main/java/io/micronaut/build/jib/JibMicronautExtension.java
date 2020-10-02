package io.micronaut.build.jib;

import com.google.cloud.tools.jib.api.buildplan.*;
import com.google.cloud.tools.jib.buildplan.UnixPathParser;
import com.google.cloud.tools.jib.maven.extension.JibMavenPluginExtension;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import io.micronaut.build.MicronautRuntime;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

import java.util.*;
import java.util.stream.Collectors;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class JibMicronautExtension implements JibMavenPluginExtension<Void> {

    @Override
    public Optional<Class<Void>> getExtraConfigType() {
        return Optional.empty();
    }

    @Override
    public ContainerBuildPlan extendContainerBuildPlan(ContainerBuildPlan buildPlan, Map<String, String> properties,
                                                       Optional<Void> extraConfig, MavenData mavenData,
                                                       ExtensionLogger logger) throws JibPluginExtensionException {
        //TODO make a best-effort guess
        MicronautRuntime runtime = MicronautRuntime.valueOf(properties.getOrDefault("micronautRuntime", "NONE"));
        ContainerBuildPlan.Builder builder = buildPlan.toBuilder();

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


}
