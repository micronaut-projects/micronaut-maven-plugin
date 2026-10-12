package io.micronaut.maven;

import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.eclipse.aether.RepositorySystem;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
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
