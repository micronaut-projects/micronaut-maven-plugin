package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.api.buildplan.AbsoluteUnixPath;
import com.google.cloud.tools.jib.api.buildplan.ContainerBuildPlan;
import com.google.cloud.tools.jib.api.buildplan.FileEntriesLayer;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.LayerObject;
import com.google.cloud.tools.jib.api.buildplan.Platform;
import com.google.cloud.tools.jib.cache.Cache;
import com.google.cloud.tools.jib.image.json.ContainerConfigurationTemplate;
import com.google.cloud.tools.jib.image.json.ImageMetadataTemplate;
import com.google.cloud.tools.jib.image.json.ManifestAndConfigTemplate;
import com.google.cloud.tools.jib.image.json.V22ManifestListTemplate;
import com.google.cloud.tools.jib.image.json.V22ManifestTemplate;
import com.google.cloud.tools.jib.json.JsonTemplateMapper;
import com.google.cloud.tools.jib.maven.extension.MavenData;
import com.google.cloud.tools.jib.plugins.extension.ExtensionLogger;
import com.google.cloud.tools.jib.plugins.extension.JibPluginExtensionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junitpioneer.jupiter.SetSystemProperty;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class JdkAotCachePlanTest {

    private static final String ARM64_DIGEST = "sha256:" + "b".repeat(64);
    private static final String AMD64_DIGEST = "sha256:" + "a".repeat(64);
    private static final List<String> ENTRYPOINT = List.of("java", "-XX:+UseSerialGC", "-cp", "@/app/jib-classpath-file", "example.Application");

    private final List<String> logs = new ArrayList<>();

    @Test
    @SetSystemProperty(key = "os.arch", value = "aarch64")
    void withoutThePropertiesTheExtensionAddsTheDetectedPlatform() throws JibPluginExtensionException {
        var plan = extend(javaPlan(ENTRYPOINT).addPlatform("amd64", "linux").build(), new Properties());

        assertEquals(Set.of(new Platform("amd64", "linux"), new Platform("arm64", "linux")), plan.getPlatforms());
        assertEquals(ENTRYPOINT, plan.getEntrypoint());
        assertFalse(logs.stream().anyMatch(line -> line.startsWith("JDK AOT cache")));
    }

    @Test
    @SetSystemProperty(key = "os.arch", value = "aarch64")
    void trainingBuildTargetsOnlyTheDaemonPlatform() throws JibPluginExtensionException {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/amd64");

        var plan = extend(javaPlan(ENTRYPOINT).addPlatform("amd64", "linux").addPlatform("arm64", "linux").build(), properties);

        assertEquals(Set.of(new Platform("amd64", "linux")), plan.getPlatforms());
        assertEquals(ENTRYPOINT, plan.getEntrypoint());
        assertEquals(1, plan.getLayers().size());
    }

    @Test
    void requiresAnExplicitGarbageCollector() {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        var plan = javaPlan(List.of("java", "-Xmx512m", "-cp", "@/app/jib-classpath-file", "example.Application")).build();

        var e = assertThrows(JibPluginExtensionException.class, () -> extend(plan, properties));

        assertTrue(e.getMessage().contains("explicit garbage collector in the Jib container.jvmFlags"));
    }

    @Test
    void acceptsAGarbageCollectorFromTheImageEnvironment() throws JibPluginExtensionException {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        var plan = javaPlan(List.of("java", "-cp", "@/app/jib-classpath-file", "example.Application"))
            .addEnvironmentVariable("JAVA_TOOL_OPTIONS", "-Xss512k -XX:+UseG1GC")
            .build();

        assertEquals(Set.of(new Platform("arm64", "linux")), extend(plan, properties).getPlatforms());
    }

    @Test
    void requiresTheJavaEntrypoint() {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        var plan = javaPlan(List.of("/app/start.sh")).build();

        var e = assertThrows(JibPluginExtensionException.class, () -> extend(plan, properties));

        assertTrue(e.getMessage().contains("needs Jib's default java entrypoint"));
    }

    @Test
    void rejectsOtherRuntimes() {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        properties.setProperty("micronaut.runtime", "lambda");

        var e = assertThrows(JibPluginExtensionException.class, () -> extend(javaPlan(ENTRYPOINT).build(), properties));

        assertTrue(e.getMessage().contains("only supports the default runtime"));
    }

    @Test
    void finalBuildAddsTheCacheAsTheLastLayerAndStartsJavaWithIt(@TempDir Path tempDir) throws IOException, JibPluginExtensionException {
        Path cacheFile = Files.writeString(tempDir.resolve("app.aot"), "cache");
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        properties.setProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY, cacheFile.toString());
        var entrypoint = List.of("java", "-XX:+UseSerialGC", "-cp", "@/opt/app/jib-classpath-file", "example.Application");

        var plan = extend(javaPlan(entrypoint).build(), properties);

        assertEquals(List.of("java", "-XX:AOTCache=/opt/app/app.aot", "-XX:+UseSerialGC", "-cp", "@/opt/app/jib-classpath-file",
            "example.Application"), plan.getEntrypoint());
        assertEquals(2, plan.getLayers().size());
        var cacheLayer = (FileEntriesLayer) plan.getLayers().get(1);
        assertEquals(JdkAotCachePlan.LAYER_NAME, cacheLayer.getName());
        var entry = cacheLayer.getEntries().get(0);
        assertEquals(cacheFile, entry.getSourceFile());
        assertEquals(AbsoluteUnixPath.get("/opt/app/app.aot"), entry.getExtractionPath());
        assertEquals(FileEntriesLayer.DEFAULT_MODIFICATION_TIME, entry.getModificationTime());
        assertEquals("eclipse-temurin:25-jre", plan.getBaseImage());
    }

    @Test
    void appRootDefaultsToJibDefault() {
        assertEquals(AbsoluteUnixPath.get("/app"), JdkAotCachePlan.appRoot(List.of("java", "-cp", "/app/classpath/*", "Main")));
        assertEquals(AbsoluteUnixPath.get("/srv"), JdkAotCachePlan.appRoot(ENTRYPOINT.stream()
            .map(argument -> argument.replace("/app/", "/srv/")).toList()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"-XX:+UseSerialGC", "-XX:+UseParallelGC", "-XX:+UseG1GC", "-XX:+UseZGC", "-XX:+UseShenandoahGC", "-XX:+UseEpsilonGC"})
    void recognizesGarbageCollectorFlags(String flag) {
        assertTrue(JdkAotCachePlan.selectsGarbageCollector(List.of("java", flag), Map.of()));
        assertTrue(JdkAotCachePlan.selectsGarbageCollector(List.of("java"), Map.of("JDK_JAVA_OPTIONS", flag)));
        assertFalse(JdkAotCachePlan.selectsGarbageCollector(List.of("java", flag.replace('+', '-')), Map.of()));
    }

    @Test
    void finalBuildPinsTheBaseImageToThePlatformManifestTheTrainingBuildPulled(@TempDir Path tempDir) throws Exception {
        Path cacheDirectory = tempDir.resolve("jib-cache");
        writeManifestListMetadata(cacheDirectory, "eclipse-temurin:25-jre");
        Path cacheFile = Files.writeString(tempDir.resolve("app.aot"), "cache");
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        properties.setProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY, cacheFile.toString());
        properties.setProperty(JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY, "true");
        properties.setProperty(JdkAotCachePlan.BASE_IMAGE_CACHE_PROPERTY, cacheDirectory.toString());

        var plan = extend(javaPlan(ENTRYPOINT).build(), properties);

        assertEquals("eclipse-temurin:25-jre@" + ARM64_DIGEST, plan.getBaseImage());
    }

    @Test
    void pinnedBaseImageLooksUpThePlatformManifest(@TempDir Path tempDir) throws Exception {
        Path cacheDirectory = tempDir.resolve("jib-cache");
        writeManifestListMetadata(cacheDirectory, "eclipse-temurin:25-jre");
        var arm64 = new Platform("arm64", "linux");

        assertEquals(Optional.of("eclipse-temurin:25-jre@" + ARM64_DIGEST),
            JdkAotCachePlan.pinnedBaseImage("eclipse-temurin:25-jre", arm64, cacheDirectory));
        assertEquals(Optional.of("eclipse-temurin:25-jre@" + AMD64_DIGEST),
            JdkAotCachePlan.pinnedBaseImage("eclipse-temurin:25-jre", new Platform("amd64", "linux"), cacheDirectory));
        assertEquals(Optional.empty(), JdkAotCachePlan.pinnedBaseImage("eclipse-temurin:21-jre", arm64, cacheDirectory));
        assertEquals(Optional.empty(), JdkAotCachePlan.pinnedBaseImage("eclipse-temurin:25-jre@" + ARM64_DIGEST, arm64, cacheDirectory));
        assertEquals(Optional.empty(), JdkAotCachePlan.pinnedBaseImage("eclipse-temurin:25-jre", arm64, tempDir.resolve("missing")));
        assertEquals(Optional.empty(), JdkAotCachePlan.pinnedBaseImage("scratch", arm64, cacheDirectory));
    }

    @Test
    void finalBuildWarnsWhenItCannotPinTheBaseImage(@TempDir Path tempDir) throws Exception {
        Path cacheFile = Files.writeString(tempDir.resolve("app.aot"), "cache");
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "linux/arm64");
        properties.setProperty(JdkAotCachePlan.CACHE_FILE_PROPERTY, cacheFile.toString());
        properties.setProperty(JdkAotCachePlan.PIN_BASE_IMAGE_PROPERTY, "true");
        properties.setProperty(JdkAotCachePlan.BASE_IMAGE_CACHE_PROPERTY, tempDir.resolve("empty-cache").toString());

        var plan = extend(javaPlan(ENTRYPOINT).build(), properties);

        assertEquals("eclipse-temurin:25-jre", plan.getBaseImage());
        assertTrue(logs.contains("JDK AOT cache: could not find the digest of eclipse-temurin:25-jre that the training build pulled, "
            + "so the base image is not pinned"));
    }

    @Test
    void rejectsAnInvalidPlatform() {
        var properties = new Properties();
        properties.setProperty(JdkAotCachePlan.PLATFORM_PROPERTY, "arm64");

        assertThrows(JibPluginExtensionException.class, () -> extend(javaPlan(ENTRYPOINT).build(), properties));
    }

    private static void writeManifestListMetadata(Path cacheDirectory, String image) throws Exception {
        var manifestList = JsonTemplateMapper.readJson("""
            {"schemaVersion": 2, "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json", "manifests": [
              {"mediaType": "application/vnd.docker.distribution.manifest.v2+json", "digest": "%s", "size": 1,
               "platform": {"architecture": "amd64", "os": "linux"}},
              {"mediaType": "application/vnd.docker.distribution.manifest.v2+json", "digest": "%s", "size": 1,
               "platform": {"architecture": "arm64", "os": "linux"}}
            ]}""".formatted(AMD64_DIGEST, ARM64_DIGEST), V22ManifestListTemplate.class);
        var manifest = JsonTemplateMapper.readJson("""
            {"schemaVersion": 2, "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
             "config": {"mediaType": "application/vnd.docker.container.image.v1+json", "digest": "sha256:%s", "size": 1},
             "layers": []}""".formatted("c".repeat(64)), V22ManifestTemplate.class);
        var config = JsonTemplateMapper.readJson("{\"architecture\": \"arm64\", \"os\": \"linux\"}", ContainerConfigurationTemplate.class);
        Cache.withDirectory(cacheDirectory).writeMetadata(ImageReference.parse(image),
            new ImageMetadataTemplate(manifestList, List.of(new ManifestAndConfigTemplate(manifest, config, ARM64_DIGEST))));
    }

    private static ContainerBuildPlan.Builder javaPlan(List<String> entrypoint) {
        LayerObject layer = FileEntriesLayer.builder()
            .setName("dependencies")
            .addEntry(Path.of("dep.jar"), AbsoluteUnixPath.get("/app/libs/dep.jar"), FilePermissions.DEFAULT_FILE_PERMISSIONS,
                FileEntriesLayer.DEFAULT_MODIFICATION_TIME)
            .build();
        return ContainerBuildPlan.builder()
            .setBaseImage("eclipse-temurin:25-jre")
            .setEntrypoint(entrypoint)
            .addLayer(layer);
    }

    private ContainerBuildPlan extend(ContainerBuildPlan plan, Properties properties) throws JibPluginExtensionException {
        var project = mock(MavenProject.class);
        when(project.getProperties()).thenReturn(properties);
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.getSystemProperties()).thenReturn(new Properties());
        var mavenData = new MavenData() {
            @Override
            public MavenProject getMavenProject() {
                return project;
            }

            @Override
            public MavenSession getMavenSession() {
                return session;
            }
        };
        ExtensionLogger logger = (level, message) -> logs.add(message);
        return new JibMicronautExtension().extendContainerBuildPlan(plan, Map.of(), Optional.empty(), mavenData, logger);
    }
}
