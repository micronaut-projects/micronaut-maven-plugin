package io.micronaut.build.examples;

import io.micronaut.context.annotation.Value;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@MicronautTest
class ReactorResolverTest {

    @Value("${test.resource.from.reactor}")
    String message;

    @Test
    @DisplayName("A sibling module custom resolver can be used through testResourcesDependencies")
    void resolvesPropertyFromSiblingModule() {
        assertEquals("hello-from-reactor", message);
    }
}
