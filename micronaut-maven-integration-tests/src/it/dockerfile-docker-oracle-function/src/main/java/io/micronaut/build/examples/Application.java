package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;
import io.micronaut.build.examples.greeter.Greeter;

public class Application {

    public static void main(String[] args) {
        Micronaut.run(Application.class);
    }
}