package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfiguration.PlatformConfiguration;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class JdkAotCacheDefaultTest {

    private static final String JAR_PLUGIN = "org.apache.maven.plugins:maven-jar-plugin";
    private static final String JIB_PLUGIN = "com.google.cloud.tools:jib-maven-plugin";

    @TempDir
    Path tempDir;

    @Test
    void aJibBuildWithItsDefaultsTrainsByDefault() {
        assertNull(jibGuard(project(null), new Properties()));
    }

    @Test
    void aJibEntrypointKeepsTheImage() {
        assertEquals("the Jib container.entrypoint is set, and the cache needs Jib's default java entrypoint",
            JdkAotCacheDefault.jibGuard(List.of("/start.sh"), project(null), new Properties(), new Properties()));
    }

    @Test
    void anExplodedContainerizingModeKeepsTheImageWhereverItIsSet() throws Exception {
        var userProperties = new Properties();
        userProperties.setProperty("jib.containerizingMode", "exploded");
        assertTrue(jibGuard(project(null), userProperties).startsWith("the Jib containerizing mode is exploded"));

        var project = project(null);
        project.getProperties().setProperty("jib.containerizingMode", "exploded");
        assertTrue(jibGuard(project, new Properties()).startsWith("the Jib containerizing mode is exploded"));

        var configured = project("<configuration><containerizingMode>exploded</containerizingMode></configuration>");
        assertTrue(jibGuard(configured, new Properties()).startsWith("the Jib containerizing mode is exploded"));

        var packaged = project("<configuration><containerizingMode>packaged</containerizingMode></configuration>");
        assertNull(jibGuard(packaged, new Properties()));
    }

    @Test
    void anExtraDirectoryFileInAClassPathDirectoryKeepsTheImage() throws Exception {
        Files.createDirectories(tempDir.resolve("src/main/jib/app/resources"));
        Files.writeString(tempDir.resolve("src/main/jib/app/resources/logback.xml"), "<configuration/>");

        assertEquals("the Jib extra directories put /app/resources/logback.xml in a class path directory of Jib's "
            + "exploded mode, which the packaged mode of the cache leaves out of the class path",
            jibGuard(project(null), new Properties()));
    }

    @Test
    void anExtraDirectoryElsewhereTrainsByDefault() throws Exception {
        Files.createDirectories(tempDir.resolve("src/main/jib/etc"));
        Files.writeString(tempDir.resolve("src/main/jib/etc/app.conf"), "");
        Files.createDirectories(tempDir.resolve("extra"));
        Files.writeString(tempDir.resolve("extra/logback.xml"), "");

        assertNull(jibGuard(project(null), new Properties()));
        // A configured directory replaces src/main/jib, and its into decides where the files go
        var project = project("<configuration><extraDirectories><paths><path><from>extra</from><into>/opt/resources</into>"
            + "</path></paths></extraDirectories></configuration>");
        assertNull(jibGuard(project, new Properties()));
        var classes = project("<configuration><container><appRoot>/opt</appRoot></container><extraDirectories><paths><path>"
            + "<from>extra</from><into>/opt/resources</into></path></paths></extraDirectories></configuration>");
        assertTrue(jibGuard(classes, new Properties()).startsWith("the Jib extra directories put /opt/resources/logback.xml"));
    }

    @Test
    void theJarPluginCanComeFromThePluginManagement() {
        var project = mock(MavenProject.class);
        var build = new Build();
        var management = new PluginManagement();
        build.setPluginManagement(management);
        when(project.getBuild()).thenReturn(build);
        assertEquals(false, JdkAotCacheDefault.hasJarPlugin(project));

        var plugin = new Plugin();
        plugin.setGroupId("org.apache.maven.plugins");
        plugin.setArtifactId("maven-jar-plugin");
        management.addPlugin(plugin);
        assertTrue(JdkAotCacheDefault.hasJarPlugin(project));
    }

    @Test
    void aProjectWithoutTheJarPluginKeepsTheImage() {
        var project = project(null);
        when(project.getPlugin(JAR_PLUGIN)).thenReturn(null);

        assertEquals("the project does not declare the maven-jar-plugin, which builds the application JAR of the cache",
            jibGuard(project, new Properties()));
    }

    @Test
    void dockerBuildOnlyNeedsTheDaemonPlatformAmongThePlatforms() {
        assertNull(JdkAotCacheDefault.platformGuard("linux/arm64", Set.of(), "dockerBuild", "aarch64"));
        assertNull(JdkAotCacheDefault.platformGuard("linux/arm64",
            Set.of(platform("amd64"), platform("arm64")), "dockerBuild", "amd64"));
        assertEquals("the Jib from.platforms [linux/amd64] do not include linux/arm64, the platform of the Docker "
            + "daemon that trains the cache",
            JdkAotCacheDefault.platformGuard("linux/arm64", Set.of(platform("x86_64")), "dockerBuild", "aarch64"));
    }

    @Test
    void aRegistryBuildKeepsTheImageWhenItWouldLoseAPlatform() {
        // Jib's default platform plus the one that the extension detects
        assertNull(JdkAotCacheDefault.platformGuard("linux/amd64", Set.of(), "build", "x86_64"));
        assertEquals("the image is built for [linux/amd64, linux/arm64], and a cache only works on linux/arm64, the "
            + "platform of the Docker daemon that trains it",
            JdkAotCacheDefault.platformGuard("linux/arm64", Set.of(), "build", "aarch64"));
        assertNull(JdkAotCacheDefault.platformGuard("linux/arm64", Set.of(platform("arm64")), "buildTar", "aarch64"));
        assertTrue(JdkAotCacheDefault.platformGuard("linux/arm64", Set.of(platform("arm64"), platform("amd64")),
            "build", "aarch64").startsWith("the image is built for"));
    }

    private String jibGuard(MavenProject project, Properties userProperties) {
        return JdkAotCacheDefault.jibGuard(List.of(), project, userProperties, new Properties());
    }

    private MavenProject project(String jibConfiguration) {
        var project = mock(MavenProject.class);
        when(project.getBasedir()).thenReturn(tempDir.toFile());
        when(project.getProperties()).thenReturn(new Properties());
        when(project.getPlugin(JAR_PLUGIN)).thenReturn(new Plugin());
        if (jibConfiguration != null) {
            var jib = new Plugin();
            try {
                jib.setConfiguration(Xpp3DomBuilder.build(new StringReader(jibConfiguration)));
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
            when(project.getPlugin(JIB_PLUGIN)).thenReturn(jib);
        }
        return project;
    }

    private static PlatformConfiguration platform(String architecture) {
        return new PlatformConfiguration(Optional.of(architecture), Optional.of("linux"));
    }
}
