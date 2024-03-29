package io.micronaut.maven.examples;

import io.micronaut.runtime.Micronaut;
import io.micronaut.maven.examples.greeter.Greeter;

public class Application {

    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        System.out.println(greeter.greet("world!"));
        Micronaut.run(Application.class);
    }
}