package io.micronaut.build.examples;

import io.micronaut.context.ApplicationContext;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.stream.Stream;

/**
 * Drives the watch mode of mn:run: the first launch changes a resource, which restarts it; the second one waits for
 * the CDS archive and changes the resource again; the third one stops Maven, as Ctrl+C would.
 */
public class Application {

    private static final long ARCHIVE_TIMEOUT_MILLIS = 180_000;

    public static void main(String[] args) throws Exception {
        // mn:run runs the application in the target directory
        Path target = Path.of("").toAbsolutePath();
        Path launches = target.resolve("launches.txt");
        Path trigger = target.resolveSibling("src/main/resources/trigger.txt");
        int launch;
        try (ApplicationContext context = ApplicationContext.run()) {
            launch = Files.exists(launches) ? Files.readAllLines(launches).size() + 1 : 1;
            Files.writeString(launches, ProcessHandle.current().pid() + System.lineSeparator(),
                StandardOpenOption.CREATE, StandardOpenOption.APPEND);
            System.out.println("Launch " + launch + " started");
        }
        if (launch == 1) {
            // after the plugin's quiet period that follows a compilation
            Thread.sleep(2_000);
            Files.writeString(trigger, "1");
        } else if (launch == 2) {
            long deadline = System.currentTimeMillis() + ARCHIVE_TIMEOUT_MILLIS;
            while (!hasArchive(target.resolve("mn-cds"))) {
                if (System.currentTimeMillis() > deadline) {
                    System.out.println("Timed out waiting for the CDS archive");
                    stopMaven();
                }
                Thread.sleep(200);
            }
            Thread.sleep(1_000);
            Files.writeString(trigger, "2");
        } else {
            System.out.println("Launch 3 stops Maven");
            stopMaven();
        }
        Thread.sleep(Long.MAX_VALUE);
    }

    private static boolean hasArchive(Path directory) throws Exception {
        if (!Files.isDirectory(directory)) {
            return false;
        }
        try (Stream<Path> files = Files.list(directory)) {
            return files.anyMatch(file -> file.getFileName().toString().endsWith(".jsa"));
        }
    }

    private static void stopMaven() {
        // SIGTERM to the Maven JVM, which runs its shutdown hooks as on Ctrl+C
        ProcessHandle.current().parent().ifPresent(ProcessHandle::destroy);
    }
}
