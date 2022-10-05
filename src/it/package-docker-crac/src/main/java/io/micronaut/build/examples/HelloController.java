package io.micronaut.build.examples;

import io.micronaut.core.annotation.Nullable;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

@Controller
public class HelloController {

    @Get("/hello{/name}")
    public String hello(@Nullable String name) {
        return "Hello " + (name == null ? "World" : name) + "!";
    }
}