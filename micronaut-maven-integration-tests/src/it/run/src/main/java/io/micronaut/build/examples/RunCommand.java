package io.micronaut.build.examples;

import io.micronaut.configuration.picocli.PicocliRunner;

import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

@Command(name = "run2", description = "...",
        mixinStandardHelpOptions = true)
public class RunCommand implements Runnable {

    @Option(names = {"-v", "--verbose"}, description = "...")
    boolean verbose;

    public static void main(String[] args) throws Exception {
        PicocliRunner.run(RunCommand.class, args);
    }

    public void run() {
        // business logic here
        if (verbose) {
            System.out.println("Hi!");
            System.out.println("jmxremote property set: " + (System.getProperty("com.sun.management.jmxremote") != null));
            // The JMX agent, when the JVM starts it before main, opens a local
            // RMI connector, which accepts connections on a thread of its own
            boolean jmxAgentStarted = Thread.getAllStackTraces().keySet().stream()
                .anyMatch(thread -> thread.getName().startsWith("RMI TCP Accept"));
            System.out.println("JMX agent started: " + jmxAgentStarted);
        }
    }
}
