package io.micronaut.build.examples;

import io.micronaut.context.ApplicationContext;

public class Application {

    public static void main(String[] args) {
        try (ApplicationContext context = ApplicationContext.run()) {
            System.out.println("Application started: " + context.getClass().getName());
        }
    }
}
