package io.micronaut.maven.examples.greeter;

import jakarta.inject.Singleton;

@Singleton
public class Greeter {

    public String greet(String name) {
        return "HELLO " + name;
    }
}
