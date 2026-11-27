package testmulti.app;

import org.junit.jupiter.api.Test;
import testmulti.lib.GreeterFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GreetingTest {

    @Test
    void greetsThroughTheLibraryFixture() {
        assertEquals("Hello reactor", GreeterFixture.greeter().greet("reactor"));
    }
}
