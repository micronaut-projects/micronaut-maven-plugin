package devreload;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Produces;

/**
 * The process the application runs in: an edit reloads it in place, in the same process.
 */
@Controller("/pid")
public class PidController {
    @Get
    @Produces(MediaType.TEXT_PLAIN)
    public String pid() {
        return String.valueOf(ProcessHandle.current().pid());
    }
}
