package io.micronaut.maven.jdkaotcache;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TrainingModeTest {

    @Test
    void theIdsAreTheValuesOfTheMicronautProperty() {
        assertEquals("load", TrainingMode.LOAD.id());
        assertEquals("start", TrainingMode.START.id());
    }

    @Test
    void parsesTheModeInAnyCase() throws MojoExecutionException {
        assertEquals(Optional.of(TrainingMode.LOAD), TrainingMode.parse("load"));
        assertEquals(Optional.of(TrainingMode.LOAD), TrainingMode.parse(" LOAD "));
        assertEquals(Optional.of(TrainingMode.START), TrainingMode.parse("start"));
        assertEquals(Optional.of(TrainingMode.START), TrainingMode.parse("Start"));
    }

    @Test
    void noValueIsNoMode() throws MojoExecutionException {
        assertEquals(Optional.empty(), TrainingMode.parse(null));
        assertEquals(Optional.empty(), TrainingMode.parse(""));
        assertEquals(Optional.empty(), TrainingMode.parse("  "));
    }

    @Test
    void anotherValueFailsAndNamesTheOption() {
        var e = assertThrows(MojoExecutionException.class, () -> TrainingMode.parse("laod"));

        assertEquals("Invalid micronaut.docker.jdkAotCache.trainingMode 'laod': it must be load or start", e.getMessage());
    }
}
