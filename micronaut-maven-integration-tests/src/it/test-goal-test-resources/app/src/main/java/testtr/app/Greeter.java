package testtr.app;

import io.micronaut.context.annotation.Value;
import jakarta.inject.Singleton;

@Singleton
public class Greeter {
    private final String greeting;

    public Greeter(@Value("${greeting.message}") String greeting) {
        this.greeting = greeting;
    }

    public String greeting() {
        return greeting;
    }
}
