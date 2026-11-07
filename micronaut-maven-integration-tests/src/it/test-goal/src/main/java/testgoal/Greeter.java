package testgoal;

import jakarta.inject.Singleton;

@Singleton
public class Greeter {

    public String greet(String name) {
        return "Hello " + name;
    }
}
