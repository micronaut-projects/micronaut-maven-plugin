package testgoal;

import io.micronaut.context.ApplicationContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GreeterTest {

    @Test
    void greetsFromTheContext() {
        try (ApplicationContext context = ApplicationContext.run("test")) {
            Greeter greeter = context.getBean(Greeter.class);
            assertEquals("Hello tests", greeter.greet(context.getRequiredProperty("greeting.name", String.class)));
        }
    }
}
