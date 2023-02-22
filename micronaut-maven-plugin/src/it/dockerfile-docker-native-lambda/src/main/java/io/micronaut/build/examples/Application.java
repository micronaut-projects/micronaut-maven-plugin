package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;
import io.micronaut.build.examples.greeter.Greeter;

public class Application {

    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        System.out.println(greeter.greet("world!"));
        Micronaut.run(Application.class);
    }
}