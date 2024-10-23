package io.micronaut.maven.jib;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.google.cloud.tools.jib.api.buildplan.FilePermissions;
import com.google.cloud.tools.jib.api.buildplan.ImageFormat;
import com.google.cloud.tools.jib.configuration.ContainerConfiguration;
import com.google.cloud.tools.jib.maven.JibPluginConfiguration;
import com.google.cloud.tools.jib.maven.JibPluginConfiguration.FromConfiguration;
import com.google.cloud.tools.jib.maven.JibPluginConfiguration.ToConfiguration;
import com.google.cloud.tools.jib.maven.MavenRawConfiguration;
import com.google.cloud.tools.jib.plugins.common.AuthProperty;
import com.google.cloud.tools.jib.plugins.common.RawConfiguration;
import io.micronaut.core.annotation.Creator;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class JibConfigurationServiceTest {

    @Test
    void testGetToImage() throws XmlPullParserException, IOException {
        var config = """
                <configuration>
                  <from>
                    <image>openjdk:alpine</image>
                  </from>
                  <to>
                    <image>localhost:5000/my-image:built-with-jib</image>
                    <tags>
                      <tag>tag2</tag>
                      <tag>latest</tag>
                    </tags>
                  </to>
                  <container>
                    <args>
                      <arg>some</arg>
                      <arg>args</arg>
                    </args>
                    <ports>
                      <port>1000</port>
                      <port>2000-2003/udp</port>
                    </ports>
                    <labels>
                      <key1>value1</key1>
                      <key2>value2</key2>
                    </labels>
                  </container>
                </configuration>""";
        var parsed = parseConfiguration(config);
        var toString = parsed.toString();
        final XmlMapper mapper = XmlMapper.builder().findAndAddModules().build();
        var serialized = mapper.readValue(toString, JibConfiguration.class);

        assertNotNull(serialized);
    }

//    private JibConfigurationService setupJibConfigurationService(String xmlConfiguration) throws XmlPullParserException, IOException {
//        var configuration = parseConfiguration(xmlConfiguration);
//        var jibPlugin = mock(Plugin.class);
//        when(jibPlugin.getConfiguration()).thenReturn(configuration);
//        var project = mock(MavenProject.class);
//        when(project.getPlugin(MavenProjectProperties.PLUGIN_KEY)).thenReturn(jibPlugin);
//    }

    private Xpp3Dom parseConfiguration(String xmlConfiguration) throws XmlPullParserException, IOException {
        return Xpp3DomBuilder.build(new StringReader(xmlConfiguration));
    }

}