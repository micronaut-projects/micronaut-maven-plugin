/*
 * Copyright 2017-2026 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven.dev;

import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * The lock of a project's class outputs that Micronaut Core's development launcher takes, {@code io.micronaut.dev.compile.OutputLock}:
 * {@code mn:dev} and {@code mn:test} run at once on one project both write {@code target/classes}, and the launcher
 * of each compiles, and reads the outputs, holding a {@link FileChannel#lock() file lock} on
 * {@code target/classes.micronaut-dev.lock}, the first of the application's class outputs with
 * {@value #SUFFIX} appended. A compilation Maven runs for either goal, the lifecycle forked before the launch and each
 * build-tool mode compilation, takes the same lock, so that neither process sees the other's compilation half done.
 *
 * <p>Within this JVM the lock is a semaphore per file, since the JVM holds a file lock for the process: two holds in
 * one Maven host, such as mvnd, wait for each other instead of failing.</p>
 *
 * @author graemerocher
 * @since 5.1.0
 */
public final class OutputLock {

    /**
     * The suffix of the lock file beside a class output, as the launcher names it.
     */
    public static final String SUFFIX = ".micronaut-dev.lock";

    private static final Map<Path, Semaphore> PERMITS = new ConcurrentHashMap<>();

    private OutputLock() {
    }

    /**
     * The lock files a compilation of a project takes: that of the application's output, {@code target/classes},
     * which the launcher locks whenever it compiles the application, and, when the tests are compiled too, that of
     * {@code target/test-classes}, which the launcher of {@code mn:test} locks when it compiles no application
     * source.
     *
     * @param project the project
     * @param tests whether the compilation writes the test output too
     * @return the lock files
     */
    public static List<Path> files(MavenProject project, boolean tests) {
        List<Path> files = new ArrayList<>();
        files.add(file(Path.of(project.getBuild().getOutputDirectory())));
        if (tests) {
            files.add(file(Path.of(project.getBuild().getTestOutputDirectory())));
        }
        return files;
    }

    /**
     * The lock file of a class output, as the launcher resolves it.
     *
     * @param output the class output
     * @return the file beside it
     */
    public static Path file(Path output) {
        Path normalized = output.toAbsolutePath().normalize();
        Path name = normalized.getFileName();
        return normalized.resolveSibling((name == null ? "" : name.toString()) + SUFFIX);
    }

    /**
     * Takes the locks, in the order of their paths so that two processes taking several do not deadlock, waiting for
     * another process or thread that holds one.
     *
     * @param files the lock files, created when missing
     * @param waiting told once when another holder makes the call wait
     * @return the hold, to close
     * @throws UncheckedIOException if a lock file cannot be locked
     */
    public static Hold lock(Collection<Path> files, Consumer<String> waiting) {
        TreeSet<Path> ordered = new TreeSet<>();
        for (Path file : files) {
            ordered.add(file.toAbsolutePath().normalize());
        }
        AtomicBoolean told = new AtomicBoolean();
        Consumer<Path> waitingFor = file -> {
            if (told.compareAndSet(false, true)) {
                waiting.accept("Another development process of the project is writing its class output: waiting for it (" + file + ")");
            }
        };
        List<Held> held = new ArrayList<>(ordered.size());
        try {
            for (Path file : ordered) {
                held.add(lockFile(file, waitingFor));
            }
        } catch (IOException | RuntimeException e) {
            release(held);
            if (e instanceof IOException io) {
                throw new UncheckedIOException("Cannot lock " + ordered, io);
            }
            throw (RuntimeException) e;
        }
        AtomicBoolean closed = new AtomicBoolean();
        return () -> {
            if (closed.compareAndSet(false, true)) {
                release(held);
            }
        };
    }

    private static Held lockFile(Path file, Consumer<Path> waiting) throws IOException {
        Semaphore permit = PERMITS.computeIfAbsent(file, path -> new Semaphore(1));
        if (!permit.tryAcquire()) {
            waiting.accept(file);
            try {
                permit.acquire();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted waiting for " + file, e);
            }
        }
        try {
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            FileChannel channel = FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
            try {
                FileLock taken = channel.tryLock();
                if (taken == null) {
                    waiting.accept(file);
                    channel.lock();
                }
            } catch (IOException | RuntimeException e) {
                channel.close();
                throw e;
            }
            return new Held(channel, permit);
        } catch (IOException | RuntimeException e) {
            permit.release();
            throw e;
        }
    }

    private static void release(List<Held> held) {
        // the reverse of the order taken
        for (int i = held.size() - 1; i >= 0; i--) {
            held.get(i).release();
        }
    }

    /**
     * A hold of the locks, released when closed, once.
     */
    @FunctionalInterface
    public interface Hold extends AutoCloseable {
        @Override
        void close();
    }

    private record Held(FileChannel channel, Semaphore permit) {
        void release() {
            try {
                // closing the channel releases its lock
                channel.close();
            } catch (IOException e) {
                // the process exiting releases it
            } finally {
                permit.release();
            }
        }
    }
}
