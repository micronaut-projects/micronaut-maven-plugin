package io.micronaut.build.examples;

import io.micronaut.context.annotation.Context;
import io.micronaut.context.annotation.Requires;
import io.micronaut.context.annotation.Value;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * Connects to its backend when the application starts, as a datasource does. Nothing listens on the default address,
 * so the application can only start where the backend is, which is not the image build.
 */
@Context
@Requires(property = "backend.enabled", notEquals = "false")
public class Backend {

    public Backend(@Value("${backend.host:127.0.0.1}") String host, @Value("${backend.port:1}") int port) throws IOException {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(host, port), 2000);
        }
    }
}
