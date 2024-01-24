package io.micronaut.build.examples;

import io.micronaut.crac.events.AfterRestoreEvent;
import io.micronaut.crac.events.BeforeCheckpointEvent;
import io.micronaut.runtime.event.annotation.EventListener;
import jakarta.inject.Singleton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Singleton
public class CheckpointRestoreEventsListener {

    private static final Logger LOG = LoggerFactory.getLogger(CheckpointRestoreEventsListener.class);

    @EventListener
    public void beforeCheckpoint(BeforeCheckpointEvent event) {
        LOG.info("Preparing Checkpoint...");
    }

    @EventListener
    public void afterRestore(AfterRestoreEvent event) {
        LOG.info("Restore completed!");
    }

}