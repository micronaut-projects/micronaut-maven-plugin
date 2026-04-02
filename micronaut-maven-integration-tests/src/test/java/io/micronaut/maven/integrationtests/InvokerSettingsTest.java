package io.micronaut.maven.integrationtests;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilderFactory;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class InvokerSettingsTest {

    @Test
    void settingsXmlDoesNotUseRawGithubPluginRepositories() throws Exception {
        Document document = DocumentBuilderFactory.newInstance()
            .newDocumentBuilder()
            .parse(Path.of("src/it/settings.xml").toFile());

        NodeList pluginRepositories = document.getElementsByTagName("pluginRepository");

        assertEquals(1, pluginRepositories.getLength());

        Element pluginRepository = (Element) pluginRepositories.item(0);
        assertEquals("local.central", textContent(pluginRepository, "id"));
        assertFalse(
            textContent(pluginRepository, "url")
                .contains("raw.githubusercontent.com/graalvm/native-build-tools/snapshots")
        );
    }

    private static String textContent(Element element, String tagName) {
        return element.getElementsByTagName(tagName).item(0).getTextContent().trim();
    }
}
