package testtr.app;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import jakarta.inject.Inject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@MicronautTest(startApplication = false)
class GreetingTest {
    @Inject
    Greeter greeter;

    @Test
    void greets() {
        assertEquals("Hello from the test resources server", greeter.greeting());
    }
}
