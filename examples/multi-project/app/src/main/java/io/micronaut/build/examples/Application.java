package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;

public class Application {
    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        System.out.println(greeter.greet("World"));
        Micronaut.run(Application.class, args);
    }
}