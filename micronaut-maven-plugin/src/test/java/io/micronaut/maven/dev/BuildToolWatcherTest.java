package io.micronaut.maven.dev;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BuildToolWatcherTest {

    private static final long QUIET_PERIOD_MILLIS = 200;
    private static final long TIMEOUT_NANOS = TimeUnit.SECONDS.toNanos(30);

    @TempDir
    Path sources;

    @Test
    void closingTheWatcherStopsTheWatchAndCancelsAPendingCompilation() throws Exception {
        Semaphore compilations = new Semaphore(0);
        BuildToolWatcher watcher = BuildToolWatcher.start(List.of(sources), QUIET_PERIOD_MILLIS, compilations::release);
        try {
            // the watch is live: a change compiles once the burst is quiet
            long deadline = System.nanoTime() + TIMEOUT_NANOS;
            boolean compiled = false;
            while (!compiled && System.nanoTime() < deadline) {
                touch("Change.java");
                compiled = compilations.tryAcquire(QUIET_PERIOD_MILLIS * 3, TimeUnit.MILLISECONDS);
            }
            assertTrue(compiled, "a change compiles");
        } finally {
            watcher.close();
        }
        assertTrue(watcher.isClosed());
        assertTrue(watcher.isStopped(), "the watch and the compilation threads stop with the goal");

        compilations.drainPermits();
        touch("AfterTheGoal.java");
        assertFalse(compilations.tryAcquire(QUIET_PERIOD_MILLIS * 10, TimeUnit.MILLISECONDS), "a change after the goal finished compiles nothing");
    }

    @Test
    void aCompilationPendingWhenTheGoalFinishesDoesNotRun() throws Exception {
        Semaphore compilations = new Semaphore(0);
        // a quiet period long enough that the compilation is still pending when the watcher closes
        BuildToolWatcher watcher = BuildToolWatcher.start(List.of(sources), TimeUnit.SECONDS.toMillis(2), compilations::release);
        try {
            // the watch is live, and the compilation pending, once a change was observed: well within the quiet period
            long deadline = System.nanoTime() + TIMEOUT_NANOS;
            boolean observed = false;
            while (!observed && System.nanoTime() < deadline) {
                touch("Pending.java");
                observed = watcher.awaitChange(QUIET_PERIOD_MILLIS * 3);
            }
            assertTrue(observed, "the change is observed");
        } finally {
            watcher.close();
        }
        assertTrue(watcher.isStopped());
        assertFalse(compilations.tryAcquire(3, TimeUnit.SECONDS), "the pending compilation is cancelled");
    }

    @Test
    void closingTwiceIsANoOp() throws Exception {
        BuildToolWatcher watcher = BuildToolWatcher.start(List.of(sources), QUIET_PERIOD_MILLIS, () -> { });
        watcher.close();
        watcher.close();
        assertTrue(watcher.isClosed());
        assertTrue(watcher.isStopped());
        assertFalse(watcher.awaitChange(0), "no change was observed");
    }

    private void touch(String name) {
        try {
            Files.writeString(sources.resolve(name), "class " + name.replace(".java", "") + " { long at = " + System.nanoTime() + "L; }");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
