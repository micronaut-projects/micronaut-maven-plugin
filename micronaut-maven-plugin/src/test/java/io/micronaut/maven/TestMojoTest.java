package io.micronaut.maven;

import io.micronaut.maven.dev.ControlPanel;
import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class TestMojoTest {

    @Test
    void mapsSurefireTestToPatterns() {
        assertEquals(List.of(), TestMojo.surefireFilter(null));
        assertEquals(List.of(), TestMojo.surefireFilter(" "));
        assertEquals(List.of("CalculatorTest"), TestMojo.surefireFilter("CalculatorTest"));
        assertEquals(List.of("CalculatorTest.adds"), TestMojo.surefireFilter("CalculatorTest#adds"));
        assertEquals(List.of("CalculatorTest.adds", "CalculatorTest.subtracts"), TestMojo.surefireFilter("CalculatorTest#adds+subtracts"));
        assertEquals(List.of("CalculatorTest.add*"), TestMojo.surefireFilter("CalculatorTest#add*"));
        assertEquals(List.of("com.example.*Test", "OtherTest"), TestMojo.surefireFilter("com.example.*Test, OtherTest"));
        assertEquals(List.of("com.example.FooTest"), TestMojo.surefireFilter("**/com/example/FooTest.java"));
        assertEquals(List.of("FooTest"), TestMojo.surefireFilter("**/FooTest.class"));
        assertEquals(List.of("com.example.FooTest"), TestMojo.surefireFilter("**\\com\\example\\FooTest.java"));
        assertEquals(List.of("*.adds"), TestMojo.surefireFilter("#adds"));
        assertEquals(List.of("!SlowTest", "%regex[.*Test]"), TestMojo.surefireFilter("!SlowTest,%regex[.*Test]"));
    }

    @Test
    void theTestRunsListenOnALiveReloadPortOfTheirOwn() throws Exception {
        TestMojo mojo = testMojo();
        setField(mojo, "liveReloadPort", 35729);
        setField(mojo, "testLiveReloadPort", 35730);

        // mn:dev's port does not apply: both goals run at once
        assertEquals(35730, mojo.liveReloadPort());
    }

    @Test
    void theBuildToolCompilationWritesTheTestOutputToo() throws Exception {
        TestMojo mojo = testMojo();

        assertEquals("test-compile", mojo.buildToolGoal());
        assertTrue(mojo.compilesTests());
    }

    @Test
    void theLiveReloadPortOfTheTestsIsSetWithItsOwnProperty() throws Exception {
        Element test = mojoDescriptor(TestMojo.MOJO_NAME);
        Element port = configuration(test, "testLiveReloadPort");

        assertEquals("${mn.test.livereload.port}", port.getTextContent().trim());
        assertEquals("35730", port.getAttribute("default-value"));
        // mn:dev has no such parameter
        assertFalse(hasConfiguration(mojoDescriptor(DevMojo.MOJO_NAME), "testLiveReloadPort"));
    }

    @Test
    void aCustomRunnerLeavesTheJUnitPlatformLauncherOut() throws Exception {
        assertFalse(launcherArtifactIds("custom").contains("junit-platform-launcher"));
    }

    @Test
    void theJUnitPlatformRunnerAddsItsLauncher() throws Exception {
        assertTrue(launcherArtifactIds("junit-platform").contains("junit-platform-launcher"));
    }

    @Test
    void theManifestOfTheTestsHasNoControlPanelAndNoServerSetting(@TempDir Path directory) throws Exception {
        MavenProject project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId("app");
        project.setVersion("1.0");
        project.setFile(directory.resolve("pom.xml").toFile());
        project.getBuild().setDirectory(directory.resolve("target").toString());
        project.getBuild().setOutputDirectory(directory.resolve("target/classes").toString());
        project.getBuild().setTestOutputDirectory(directory.resolve("target/test-classes").toString());
        DependencyManagement management = new DependencyManagement();
        Dependency platform = new Dependency();
        platform.setGroupId(ControlPanel.PLATFORM_GROUP);
        platform.setArtifactId("micronaut-parent");
        platform.setVersion(ControlPanel.MINIMUM_PLATFORM_VERSION);
        management.addDependency(platform);
        project.getModel().setDependencyManagement(management);
        MavenSession session = mock(MavenSession.class);
        MavenSession runnableSession = mock(MavenSession.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        when(session.clone()).thenReturn(runnableSession);
        when(session.getRepositorySession()).thenReturn(mock(RepositorySystemSession.class));
        when(session.getAllProjects()).thenReturn(List.of(project));
        when(runnableSession.getCurrentProject()).thenReturn(project);
        when(runnableSession.getSystemProperties()).thenReturn(new Properties());
        when(runnableSession.getUserProperties()).thenReturn(new Properties());
        TestMojo mojo = new TestMojo(session, mock(ToolchainManager.class), mock(CompilerService.class),
            mock(ExecutorService.class), mock(RepositorySystem.class), mock(ComponentConfigurator.class));
        DependencyResolutionService resolution = mock(DependencyResolutionService.class);
        setField(mojo, "runnableProject", project);
        setField(mojo, "mojoExecution", mock(MojoExecution.class));
        setField(mojo, "dependencyResolutionService", resolution);
        // what the goal shares with mn:dev, where these turn both on
        setField(mojo, "controlPanel", true);
        setField(mojo, "bindLoopback", true);
        setField(mojo, "strategy", "auto");
        setField(mojo, "compile", "embedded");
        setField(mojo, "runner", "junit-platform");
        setField(mojo, "selection", "affected");
        setField(mojo, "reportPath", "/report");
        Path manifestDirectory = mojo.manifestDirectory();
        // the settings of an mn:dev running beside these tests
        Path devResources = directory.resolve("target/micronaut-dev").resolve(ControlPanel.RESOURCES_DIRECTORY_NAME);
        ControlPanel.writeResources(devResources, List.of());

        DevManifest manifest = mojo.buildManifest(manifestDirectory);

        assertFalse(manifest.entries().containsKey(DevManifest.BIND_LOOPBACK));
        assertEquals("junit-platform", manifest.entries().get("micronaut.dev.test.runner"));
        verifyNoInteractions(resolution);
        assertFalse(Files.exists(manifestDirectory.resolve(ControlPanel.RESOURCES_DIRECTORY_NAME)));
        assertTrue(Files.isRegularFile(devResources.resolve(ControlPanel.PROPERTIES_RESOURCE)));
    }

    private static List<String> launcherArtifactIds(String runner) throws Exception {
        TestMojo mojo = testMojo();
        setField(mojo, "runner", runner);
        setField(mojo, "runnableProject", new MavenProject());
        return mojo.launcherArtifacts().stream().map(Artifact::getArtifactId).toList();
    }

    private static TestMojo testMojo() {
        MavenSession session = mock(MavenSession.class);
        when(session.getUserProperties()).thenReturn(new Properties());
        return new TestMojo(session, mock(ToolchainManager.class), mock(CompilerService.class), mock(ExecutorService.class),
            mock(RepositorySystem.class), mock(ComponentConfigurator.class));
    }

    private static Element mojoDescriptor(String goal) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
        try (InputStream inputStream = TestMojoTest.class.getClassLoader().getResourceAsStream("META-INF/maven/plugin.xml")) {
            assertNotNull(inputStream);
            NodeList mojos = factory.newDocumentBuilder().parse(inputStream).getElementsByTagName("mojo");
            for (int i = 0; i < mojos.getLength(); i++) {
                Element mojo = (Element) mojos.item(i);
                if (goal.equals(mojo.getElementsByTagName("goal").item(0).getTextContent().trim())) {
                    return mojo;
                }
            }
        }
        throw new IllegalArgumentException("No goal " + goal);
    }

    private static boolean hasConfiguration(Element mojo, String parameter) {
        Element configuration = (Element) mojo.getElementsByTagName("configuration").item(0);
        return configuration.getElementsByTagName(parameter).getLength() > 0;
    }

    private static Element configuration(Element mojo, String parameter) {
        Element configuration = (Element) mojo.getElementsByTagName("configuration").item(0);
        return (Element) configuration.getElementsByTagName(parameter).item(0);
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        for (Class<?> type = target.getClass(); type != null; type = type.getSuperclass()) {
            try {
                Field field = type.getDeclaredField(name);
                field.setAccessible(true);
                field.set(target, value);
                return;
            } catch (NoSuchFieldException e) {
                // declared higher up
            }
        }
        throw new NoSuchFieldException(name);
    }
}
