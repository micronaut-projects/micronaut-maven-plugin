package io.micronaut.maven.dev;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BuildToolWatcherTest {

    private static final long QUIET_PERIOD_MILLIS = 200;

    @TempDir
    Path sources;

    @Test
    void closingTheWatcherStopsTheWatchAndCancelsAPendingCompilation() throws Exception {
        AtomicInteger compilations = new AtomicInteger();
        BuildToolWatcher watcher = BuildToolWatcher.start(List.of(sources), QUIET_PERIOD_MILLIS, compilations::incrementAndGet);
        try {
            // the watch is live: a change compiles once the burst is quiet
            awaitTrue(() -> {
                touch("Change.java");
                return compilations.get() > 0;
            });
        } finally {
            watcher.close();
        }
        assertTrue(watcher.isClosed());
        assertTrue(watcher.isStopped(), "the watch and the compilation threads stop with the goal");

        int compiled = compilations.get();
        touch("AfterTheGoal.java");
        Thread.sleep(QUIET_PERIOD_MILLIS * 10);
        assertEquals(compiled, compilations.get(), "a change after the goal finished compiles nothing");
    }

    @Test
    void aCompilationPendingWhenTheGoalFinishesDoesNotRun() throws Exception {
        AtomicInteger compilations = new AtomicInteger();
        // a quiet period long enough that the compilation is still pending when the watcher closes
        BuildToolWatcher watcher = BuildToolWatcher.start(List.of(sources), TimeUnit.SECONDS.toMillis(2), compilations::incrementAndGet);
        try {
            // the watch is live, and the compilation pending, once a change was observed: well within the quiet period
            awaitTrue(() -> {
                touch("Pending.java");
                return watcher.changes() > 0;
            });
        } finally {
            watcher.close();
        }
        Thread.sleep(TimeUnit.SECONDS.toMillis(3));
        assertEquals(0, compilations.get(), "the pending compilation is cancelled");
        assertTrue(watcher.isStopped());
    }

    private void touch(String name) {
        try {
            Files.writeString(sources.resolve(name), "class " + name.replace(".java", "") + " { long at = " + System.nanoTime() + "L; }");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void awaitTrue(BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(30);
        while (!condition.getAsBoolean()) {
            if (System.nanoTime() > deadline) {
                throw new AssertionError("timed out");
            }
            Thread.sleep(QUIET_PERIOD_MILLIS * 3);
        }
    }
}
