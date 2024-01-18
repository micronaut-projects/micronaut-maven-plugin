package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;
public class Application {
    public static void main(String[] args) {
        Micronaut.run(Application.class);
        System.exit(0);
    }
}