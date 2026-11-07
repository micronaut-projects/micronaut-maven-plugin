package devreactor.app;

import devreactor.lib.Greeter;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Produces;

@Controller("/hello")
public class HelloController {
    private final Greeter greeter;

    public HelloController(Greeter greeter) {
        this.greeter = greeter;
    }

    @Get
    @Produces(MediaType.TEXT_PLAIN)
    public String index() {
        return greeter.greet();
    }
}
