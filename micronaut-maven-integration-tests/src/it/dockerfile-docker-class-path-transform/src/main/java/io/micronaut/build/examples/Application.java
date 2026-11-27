package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;

import java.util.function.Supplier;

public class Application {
    public static void main(String[] args) {
        // A lambda, so that the class path transform rewrites the classes directory too
        Supplier<Class<?>> application = () -> Application.class;
        Micronaut.run(application.get());
    }
}
