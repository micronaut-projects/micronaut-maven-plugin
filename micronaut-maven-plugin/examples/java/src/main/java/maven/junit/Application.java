package maven.junit;

import io.micronaut.runtime.Micronaut;
import maven.junit.greeter.Greeter;

public class Application {

    public static void main(String[] args) {
        Greeter greeter = new Greeter();
        System.out.println(greeter.greet("world!"));
        Micronaut.run(Application.class);
    }
}