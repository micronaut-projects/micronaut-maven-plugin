package devmulti.lib;

import devmulti.common.Greeting;
import jakarta.inject.Singleton;

@Singleton
public class Greeter {

    public Greeting greet() {
        return new Greeting("hello");
    }
}
