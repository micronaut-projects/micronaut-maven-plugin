package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;

public class Application1 {
    public static void main(String[] args) {
        Micronaut.run(Application1.class, args);
        System.exit(0);
    }
}