package io.micronaut.build.examples;

import io.micronaut.context.ApplicationContext;
import org.slf4j.LoggerFactory;

public class Application {

    public static void main(String[] args) {
        try (ApplicationContext context = ApplicationContext.run()) {
            LoggerFactory.getLogger(Application.class).info("Hello from the application");
            System.out.println("Class path: " + System.getProperty("java.class.path"));
        }
    }
}
