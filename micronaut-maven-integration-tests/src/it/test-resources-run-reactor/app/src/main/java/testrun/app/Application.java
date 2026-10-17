package testrun.app;

import io.micronaut.context.ApplicationContext;

public class Application {
    public static void main(String[] args) {
        try (ApplicationContext context = ApplicationContext.run()) {
            System.out.println("greeting=" + context.getRequiredProperty("greeting.message", String.class));
        }
    }
}
