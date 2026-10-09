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

import io.methvin.watcher.DirectoryWatcher;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Watches the source roots of the reactor in build-tool mode, and runs an action, the compilation through Maven,
 * once a burst of changes is quiet. It lives as long as the goal that started it: {@link #close() closing} it stops
 * the watch, cancels a pending action, and waits for a running one, so that no compilation is launched after the
 * goal finished, in a Maven host that outlives it, such as mvnd or an embedded Maven.
 *
 * @author graemerocher
 * @since 5.1.0
 */
public final class BuildToolWatcher implements AutoCloseable {

    private static final long STOP_TIMEOUT_SECONDS = 30;

    private final DirectoryWatcher watcher;
    private final ScheduledExecutorService scheduler;
    private final Thread thread;
    private final long quietPeriodMillis;
    private final Runnable action;
    private ScheduledFuture<?> pending;
    private volatile boolean closed;
    private final AtomicLong changes = new AtomicLong();

    private BuildToolWatcher(List<Path> paths, long quietPeriodMillis, Runnable action) throws IOException {
        this.quietPeriodMillis = quietPeriodMillis;
        this.action = action;
        this.scheduler = Executors.newSingleThreadScheduledExecutor(runnable -> {
            Thread compileThread = new Thread(runnable, "micronaut-dev-maven-compile");
            compileThread.setDaemon(true);
            return compileThread;
        });
        this.watcher = DirectoryWatcher.builder()
            .paths(paths)
            .listener(event -> changed())
            .build();
        this.thread = new Thread(watcher::watch, "micronaut-dev-maven-watcher");
        this.thread.setDaemon(true);
    }

    /**
     * Starts watching.
     *
     * @param paths the directories to watch
     * @param quietPeriodMillis how long the changes must be quiet before the action runs
     * @param action the action, run on a thread of its own, one run at a time
     * @return the watcher, to close when the goal finishes
     * @throws IOException if the directories cannot be watched
     */
    public static BuildToolWatcher start(List<Path> paths, long quietPeriodMillis, Runnable action) throws IOException {
        BuildToolWatcher buildToolWatcher = new BuildToolWatcher(paths, quietPeriodMillis, action);
        buildToolWatcher.thread.start();
        return buildToolWatcher;
    }

    /**
     * @return whether the watcher was closed: an action still running stops short of its effects
     */
    public boolean isClosed() {
        return closed;
    }

    private synchronized void changed() {
        if (closed) {
            return;
        }
        if (pending != null) {
            pending.cancel(false);
        }
        changes.incrementAndGet();
        try {
            // a burst of events from one save becomes one run
            pending = scheduler.schedule(this::run, quietPeriodMillis, TimeUnit.MILLISECONDS);
        } catch (RejectedExecutionException e) {
            // closed meanwhile
        }
    }

    private void run() {
        if (!closed) {
            action.run();
        }
    }

    /**
     * Stops the watch, cancels a pending action, and waits for a running one to finish, without interrupting it.
     */
    @Override
    public void close() {
        synchronized (this) {
            if (closed) {
                return;
            }
            closed = true;
            if (pending != null) {
                pending.cancel(false);
            }
        }
        try {
            watcher.close();
        } catch (IOException e) {
            // the watch stops with its thread
        }
        // a running compilation finishes, so that it leaves no partial build output: the pending one is cancelled
        scheduler.shutdown();
        try {
            scheduler.awaitTermination(STOP_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            thread.join(TimeUnit.SECONDS.toMillis(STOP_TIMEOUT_SECONDS));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * @return how many changes the watch observed, each of which scheduled the action
     */
    long changes() {
        return changes.get();
    }

    /**
     * @return whether the watch and the action threads stopped
     */
    boolean isStopped() {
        return scheduler.isTerminated() && !thread.isAlive();
    }
}
