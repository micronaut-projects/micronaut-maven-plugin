package io.micronaut.maven.examples;

import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.maven.examples.greeter.Greeter;
@Controller
public class DemoController {

    private final Greeter greeter;

    public DemoController(Greeter greeter) {
        this.greeter = greeter;
    }

    @Get
    public String index() {
        return greeter.greet("world!");
    }
}
