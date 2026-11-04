package io.micronaut.maven.dev;

import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.channels.FileChannel;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
    void aFileReplacedRightAfterItWasCreatedIsLockedByItsNewIdentity() throws Exception {
        Path file = directory.resolve("target/classes.micronaut-dev.lock");
        AtomicInteger opens = new AtomicInteger();

        // the file is absent, and another process's clean and compilation replace it as soon as this call created it
        try (FileChannel channel = OutputLock.lockCurrentFile(file, path -> { }, path -> {
            FileChannel opened = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
            if (opens.getAndIncrement() == 0) {
                Files.delete(path);
                Files.createFile(path);
            }
            return opened;
        }); FileChannel current = FileChannel.open(file, StandardOpenOption.WRITE)) {
            // this JVM holds the lock of the file the path names now, not of the deleted one
            assertTrue(channel.isOpen());
            assertThrows(OverlappingFileLockException.class, current::tryLock);
        }
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

    @Test
    void aLockFileAnotherProcessDeletedWhileItWasWaitedForIsLockedAgain() throws Exception {
        Path file = directory.resolve("target/classes.micronaut-dev.lock");
        Files.createDirectories(file.getParent());
        // a build's clean: the process that holds the lock deletes the file, then releases it
        Path holder = directory.resolve("Holder.java");
        Files.writeString(holder, """
            import java.nio.channels.FileChannel;
            import java.nio.file.Files;
            import java.nio.file.Path;
            import java.nio.file.StandardOpenOption;

            public class Holder {
                public static void main(String[] args) throws Exception {
                    Path file = Path.of(args[0]);
                    try (FileChannel channel = FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
                        channel.lock();
                        System.out.println("locked");
                        System.out.flush();
                        System.in.read();
                        Files.delete(file);
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
            AtomicBoolean present = new AtomicBoolean();
            CountDownLatch locked = new CountDownLatch(1);
            Thread compilation = new Thread(() -> {
                try (OutputLock.Hold hold = OutputLock.lock(List.of(file), message -> waited.set(true))) {
                    // the path names the file locked, which is what another process would lock
                    present.set(Files.isRegularFile(file));
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
            assertTrue(present.get());
        } finally {
            process.destroyForcibly();
            process.waitFor(30, TimeUnit.SECONDS);
        }
    }
}
