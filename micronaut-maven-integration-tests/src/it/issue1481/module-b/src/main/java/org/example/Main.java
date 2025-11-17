package org.example;

import io.micronaut.context.ApplicationContext;
import io.micronaut.runtime.Micronaut;

public class Main {
    public static void main(String[] args) {
        try (ApplicationContext context = Micronaut.run(Main.class, args)) {
            A bean = context.createBean(A.class);
            bean.start();
            bean.stop();
        }
    }
}
