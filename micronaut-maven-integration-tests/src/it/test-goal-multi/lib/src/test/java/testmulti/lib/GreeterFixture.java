package testmulti.lib;

public final class GreeterFixture {

    private GreeterFixture() {
    }

    public static Greeter greeter() {
        return new Greeter();
    }
}
