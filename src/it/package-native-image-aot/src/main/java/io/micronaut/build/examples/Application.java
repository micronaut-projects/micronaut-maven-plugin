package io.micronaut.build.examples;

import io.micronaut.runtime.Micronaut;
import io.micronaut.maven.examples.greeter.Greeter;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.info.*;

@OpenAPIDefinition(
    info = @Info(
            title = "app",
            version = "0.0"
    )
)
public class Application {

    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        System.out.println(greeter.greet("world!"));
        Micronaut.run(Application.class);
    }
}