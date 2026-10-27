package io.micronaut.maven.dev;

import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OutputLockTest {

    @TempDir
    Path directory;

    @Test
    void theLockFileIsBesideTheOutputAsTheLauncherNamesIt() {
        Path output = directory.resolve("app/target/./classes");

        assertEquals(directory.resolve("app/target/classes.micronaut-dev.lock").toAbsolutePath(), OutputLock.file(output));
    }

    @Test
    void aCompilationLocksTheApplicationsOutputAndTheTestsWhenItCompilesThem() {
        MavenProject project = new MavenProject();
        project.getBuild().setOutputDirectory(directory.resolve("target/classes").toString());
        project.getBuild().setTestOutputDirectory(directory.resolve("target/test-classes").toString());

        assertEquals(List.of(directory.resolve("target/classes.micronaut-dev.lock")), OutputLock.files(project, false));
        assertEquals(List.of(directory.resolve("target/classes.micronaut-dev.lock"), directory.resolve("target/test-classes.micronaut-dev.lock")),
            OutputLock.files(project, true));
    }

    @Test
    void aSecondHolderWaitsForTheFirstAndIsToldOnce() throws Exception {
        Path file = directory.resolve("target/classes.micronaut-dev.lock");
        OutputLock.Hold hold = OutputLock.lock(List.of(file), message -> { });
        assertTrue(Files.isRegularFile(file));
        AtomicReference<String> told = new AtomicReference<>();
        CountDownLatch locked = new CountDownLatch(1);
        Thread other = new Thread(() -> {
            try (OutputLock.Hold second = OutputLock.lock(List.of(file), told::set)) {
                locked.countDown();
            }
        });
        other.start();

        assertFalse(locked.await(200, TimeUnit.MILLISECONDS));
        hold.close();
        // closed once: a second close releases nothing the other holder took
        hold.close();

        assertTrue(locked.await(5, TimeUnit.SECONDS));
        other.join(5000);
        assertTrue(told.get().contains(file.toString()));
    }

    @Test
    void anotherProcessHoldingTheFileLockMakesTheCompilationWait() throws Exception {
        Path file = directory.resolve("target/classes.micronaut-dev.lock");
        Files.createDirectories(file.getParent());
        // a launcher's lock, as io.micronaut.dev.compile.OutputLock takes it: a FileChannel lock in another JVM
        Path holder = directory.resolve("Holder.java");
        Files.writeString(holder, """
            import java.nio.channels.FileChannel;
            import java.nio.file.Path;
            import java.nio.file.StandardOpenOption;

            public class Holder {
                public static void main(String[] args) throws Exception {
                    try (FileChannel channel = FileChannel.open(Path.of(args[0]), StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
                        channel.lock();
                        System.out.println("locked");
                        System.out.flush();
                        System.in.read();
                    }
                }
            }
            """);
        String java = Path.of(System.getProperty("java.home"), "bin", "java").toString();
        Process process = new ProcessBuilder(java, holder.toString(), file.toString()).redirectErrorStream(true).start();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
            assertEquals("locked", reader.readLine());
            AtomicBoolean waited = new AtomicBoolean();
            CountDownLatch locked = new CountDownLatch(1);
            Thread compilation = new Thread(() -> {
                try (OutputLock.Hold hold = OutputLock.lock(List.of(file), message -> waited.set(true))) {
                    locked.countDown();
                }
            });
            compilation.start();

            assertFalse(locked.await(300, TimeUnit.MILLISECONDS));
            assertTrue(waited.get());
            try (OutputStream in = process.getOutputStream()) {
                in.write('\n');
            }
            assertTrue(locked.await(30, TimeUnit.SECONDS));
            compilation.join(5000);
        } finally {
            process.destroyForcibly();
            process.waitFor(30, TimeUnit.SECONDS);
        }
    }
}
