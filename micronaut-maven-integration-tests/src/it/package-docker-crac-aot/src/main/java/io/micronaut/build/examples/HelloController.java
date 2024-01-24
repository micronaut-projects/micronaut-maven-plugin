package io.micronaut.build.examples;

import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

@Controller
public class HelloController {

    @Get
    public String hello() {
        return "Hello World!";
    }
}