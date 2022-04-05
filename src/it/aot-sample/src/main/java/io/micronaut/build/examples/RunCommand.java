package io.micronaut.build.examples;

import io.micronaut.configuration.picocli.PicocliRunner;
import io.micronaut.context.annotation.Property;

import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

@Command(name = "run2", description = "...",
        mixinStandardHelpOptions = true)
public class RunCommand implements Runnable {

    @Option(names = {"-v", "--verbose"}, description = "...")
    boolean verbose;

    @Property(name = "micronaut.aot.enabled")
    private boolean aotEnabled;

    public static void main(String[] args) throws Exception {
        PicocliRunner.run(RunCommand.class, args);
    }

    public void run() {
        // business logic here
        if (verbose) {
            System.out.println("Hi!");
            if (aotEnabled) {
                System.out.println("Running with AOT optimizations");
            }
        }
    }
}
