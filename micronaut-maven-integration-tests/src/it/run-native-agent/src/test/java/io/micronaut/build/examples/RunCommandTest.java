package io.micronaut.build.examples;

import io.micronaut.configuration.picocli.PicocliRunner;
import io.micronaut.context.ApplicationContext;
import io.micronaut.context.env.Environment;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class RunCommandTest {

    @Test
    public void testWithCommandLineOption() throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;

        try (PrintStream output = new PrintStream(baos);
             ApplicationContext ctx = ApplicationContext.run(Environment.CLI, Environment.TEST)) {
            System.setOut(output);
            String[] args = new String[] { "-v" };
            PicocliRunner.run(RunCommand.class, ctx, args);

            // run2
            assertTrue(baos.toString().contains("Hi!"));
        } finally {
            System.setOut(originalOut);
        }
    }
}
