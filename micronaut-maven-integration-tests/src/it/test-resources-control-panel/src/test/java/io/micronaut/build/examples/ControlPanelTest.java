package io.micronaut.build.examples;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ControlPanelTest {

    @Test
    void controlPanelIsAvailableDuringVerify() throws Exception {
        String port = Files.readString(Path.of("target/test-resources-port.txt")).trim();
        HttpRequest request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + port + "/control-panel"))
            .timeout(Duration.ofSeconds(10))
            .build();
        try (HttpClient client = HttpClient.newHttpClient()) {
            HttpResponse<Void> response = client.send(request, HttpResponse.BodyHandlers.discarding());
            assertEquals(200, response.statusCode());
        }
    }
}
