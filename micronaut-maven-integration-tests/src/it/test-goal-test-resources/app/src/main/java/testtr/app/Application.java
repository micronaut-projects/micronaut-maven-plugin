package testtr.app;

import io.micronaut.context.ApplicationContext;

public class Application {
    public static void main(String[] args) {
        try (ApplicationContext context = ApplicationContext.run()) {
            System.out.println("greeting=" + context.getBean(Greeter.class).greeting());
        }
        // the launcher would watch the sources until stopped: the application ends the run
        System.exit(0);
    }
}
