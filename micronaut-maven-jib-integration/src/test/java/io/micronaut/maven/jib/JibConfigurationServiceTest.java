package io.micronaut.maven.jib;

import com.google.cloud.tools.jib.api.Credential;
import com.google.cloud.tools.jib.api.CredentialRetriever;
import com.google.cloud.tools.jib.api.ImageReference;
import com.google.cloud.tools.jib.frontend.CredentialRetrieverFactory;
import com.google.cloud.tools.jib.maven.MavenProjectProperties;
import com.google.cloud.tools.jib.plugins.common.PropertyNames;
import com.google.cloud.tools.jib.registry.credentials.CredentialRetrievalException;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.RestoreSystemProperties;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.slf4j.Logger;

import java.io.IOException;
import java.io.StringReader;
import java.util.Optional;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
class JibConfigurationServiceTest {

    @Test
    void testGetToImageFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <to>
                    <image>localhost:5000/my-image:built-with-jib</image>
                  </to>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertTrue(service.getToImage().isPresent());

        var toImage = service.getToImage().get();

        assertEquals("localhost:5000/my-image:built-with-jib", toImage);
    }

    @Test
    @SetSystemProperty(key = PropertyNames.TO_IMAGE, value = "localhost:5000/my-image:built-with-jib")
    void testGetToImageFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertTrue(service.getToImage().isPresent());

        var toImage = service.getToImage().get();

        assertEquals("localhost:5000/my-image:built-with-jib", toImage);
    }

    @Test
    void testGetFromImageFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <from>
                    <image>openjdk:alpine</image>
                  </from>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertTrue(service.getFromImage().isPresent());

        var fromImage = service.getFromImage().get();

        assertEquals("openjdk:alpine", fromImage);
    }

    @Test
    @SetSystemProperty(key = PropertyNames.FROM_IMAGE, value = "openjdk:alpine")
    void testGetFromImageFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertTrue(service.getFromImage().isPresent());

        var fromImage = service.getFromImage().get();

        assertEquals("openjdk:alpine", fromImage);
    }

    @Test
    void testGetTagsFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <to>
                    <image>localhost:5000/my-image:built-with-jib</image>
                    <credHelper>osxkeychain</credHelper>
                    <tags>
                      <tag>tag2</tag>
                      <tag>latest</tag>
                    </tags>
                  </to>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertFalse(service.getTags().isEmpty());

        var tags = service.getTags();

        assertTrue(tags.contains("tag2"));
        assertTrue(tags.contains("latest"));
    }

    @Test
    @SetSystemProperty(key = PropertyNames.TO_TAGS, value = "tag2,latest")
    void testGetTagsFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertFalse(service.getTags().isEmpty());

        var tags = service.getTags();

        assertTrue(tags.contains("tag2"));
        assertTrue(tags.contains("latest"));
    }

    @Test
    void testGetTagsEmpty() {
        var service = setupJibConfigurationService();

        assertTrue(service.getTags().isEmpty());
    }

    @Test
    void testGetToCredentialsFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <to>
                    <image>gcr.io/my-gcp-project/my-app</image>
                    <auth>
                      <username>user</username>
                      <password>pass</password>
                    </auth>
                  </to>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertTrue(service.getToCredentials().isPresent());
        var credentials = service.getToCredentials().get();

        assertEquals("user", credentials.getUsername());
        assertEquals("pass", credentials.getPassword());
    }

    @Test
    @SetSystemProperty(key = PropertyNames.TO_AUTH_USERNAME, value = "user")
    @SetSystemProperty(key = PropertyNames.TO_AUTH_PASSWORD, value = "pass")
    void testGetToCredentialsFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertTrue(service.getToCredentials().isPresent());
        var credentials = service.getToCredentials().get();

        assertEquals("user", credentials.getUsername());
        assertEquals("pass", credentials.getPassword());
    }

    @Test
    void testGetFromCredentialsFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <from>
                    <image>aws_account_id.dkr.ecr.region.amazonaws.com/my-base-image</image>
                    <auth>
                      <username>user</username>
                      <password>pass</password>
                    </auth>
                  </from>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertTrue(service.getFromCredentials().isPresent());
        var credentials = service.getFromCredentials().get();

        assertEquals("user", credentials.getUsername());
        assertEquals("pass", credentials.getPassword());
    }

    @Test
    @SetSystemProperty(key = PropertyNames.FROM_AUTH_USERNAME, value = "user")
    @SetSystemProperty(key = PropertyNames.FROM_AUTH_PASSWORD, value = "pass")
    void testGetFromCredentialsFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertTrue(service.getFromCredentials().isPresent());
        var credentials = service.getFromCredentials().get();

        assertEquals("user", credentials.getUsername());
        assertEquals("pass", credentials.getPassword());
    }

    @Test
    void testGetWorkingDirectoryFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                    <container>
                        <workingDirectory>/foo/bar</workingDirectory>
                    </container>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertTrue(service.getWorkingDirectory().isPresent());

        var workingDirectory = service.getWorkingDirectory().get();

        assertEquals("/foo/bar", workingDirectory);
    }

    @Test
    @SetSystemProperty(key = PropertyNames.CONTAINER_WORKING_DIRECTORY, value = "/foo/bar")
    void testGetWorkingDirectoryFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertTrue(service.getWorkingDirectory().isPresent());

        var workingDirectory = service.getWorkingDirectory().get();

        assertEquals("/foo/bar", workingDirectory);
    }

    @Test
    void testGetArgsFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <container>
                    <args>
                      <arg>some</arg>
                      <arg>args</arg>
                    </args>
                  </container>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertFalse(service.getArgs().isEmpty());

        var args = service.getArgs();

        assertTrue(args.contains("some"));
        assertTrue(args.contains("args"));
    }

    @Test
    @SetSystemProperty(key = PropertyNames.CONTAINER_ARGS, value = "some,args")
    void testGetArgsFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertFalse(service.getArgs().isEmpty());

        var args = service.getArgs();

        assertTrue(args.contains("some"));
        assertTrue(args.contains("args"));
    }

    @Test
    void testGetArgsEmpty() {
        var service = setupJibConfigurationService();

        assertTrue(service.getArgs().isEmpty());
    }

    @Test
    void testGetPortsFromXml() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <container>
                    <ports>
                      <port>1000</port>
                      <port>2000-2003/udp</port>
                    </ports>
                  </container>
                </configuration>""";
        var service = setupJibConfigurationService(config);

        assertFalse(service.getPorts().isEmpty());

        var ports = service.getPorts().get();

        assertEquals("1000 2000-2003/udp", ports);
    }

    @Test
    @SetSystemProperty(key = PropertyNames.CONTAINER_PORTS, value = "1000,2000-2003/udp")
    void testGetPortsFromSystemProperties() {
        var service = setupJibConfigurationService();

        assertFalse(service.getPorts().isEmpty());

        var ports = service.getPorts().get();

        assertEquals("1000 2000-2003/udp", ports);
    }

    @Test
    void testGetPortsEmpty() {
        var service = setupJibConfigurationService();

        assertTrue(service.getPorts().isEmpty());
    }

    @Test
    void testResolveCredentialForImageInvalidImage() {
        Logger mockLogger = mock(Logger.class);
        var service = setupJibConfigurationService();

        Optional<Credential> result = service.resolveCredentialForImage("invalid::image", mockLogger);

        assertTrue(result.isEmpty());
        verify(mockLogger).warn(eq("Invalid image reference '{}': {}"), eq("invalid::image"), any(String.class));
    }

    @Test
    void testResolveCredentialForImageNoCredentials() {
        Logger mockLogger = mock(Logger.class);
        var service = setupJibConfigurationService();

        Optional<Credential> result = service.resolveCredentialForImage("localhost:5000/test:tag", mockLogger);

        assertTrue(result.isEmpty());
        // No specific log verification needed for no-creds case, as it depends on retriever failures
    }

    @Test
    void testResolveCredentialForImageWithCredentials() throws CredentialRetrievalException {
        Logger mockLogger = mock(Logger.class);
        var service = setupJibConfigurationService();

        try (var mockedStatic = mockStatic(CredentialRetrieverFactory.class)) {
            var mockFactory = mock(CredentialRetrieverFactory.class);
            var mockRetriever = mock(CredentialRetriever.class);
            var mockCredential = Credential.from("testuser", "testpass");

            mockedStatic.when(() -> CredentialRetrieverFactory.forImage(any(ImageReference.class), any(Consumer.class)))
                .thenReturn(mockFactory);
            when(mockFactory.wellKnownCredentialHelpers()).thenReturn(mockRetriever);
            when(mockRetriever.retrieve()).thenReturn(Optional.of(mockCredential));

            Optional<Credential> result = service.resolveCredentialForImage("docker.io/library/nginx:latest", mockLogger);

            assertTrue(result.isPresent());
            assertEquals("testuser", result.get().getUsername());
            assertEquals("testpass", result.get().getPassword());
        }
    }

    private JibConfigurationService setupJibConfigurationService(String xmlConfiguration) throws XmlPullParserException, IOException {
        var configuration = parseConfiguration(xmlConfiguration);
        return setupJibConfigurationService(configuration);
    }

    private JibConfigurationService setupJibConfigurationService() {
        return setupJibConfigurationService(new Xpp3Dom("configuration"));
    }

    private JibConfigurationService setupJibConfigurationService(Xpp3Dom configuration) {
        var jibPlugin = mock(Plugin.class);
        when(jibPlugin.getConfiguration()).thenReturn(configuration);
        var project = mock(MavenProject.class);
        when(project.getPlugin(MavenProjectProperties.PLUGIN_KEY)).thenReturn(jibPlugin);
        return new JibConfigurationService(project);
    }

    private Xpp3Dom parseConfiguration(String xmlConfiguration) throws XmlPullParserException, IOException {
        return Xpp3DomBuilder.build(new StringReader(xmlConfiguration));
    }

}
