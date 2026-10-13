package io.micronaut.maven.jdkaotcache;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TrainingModeTest {

    @Test
    void theIdsAreTheValuesOfTheMicronautProperty() {
        assertEquals("load", TrainingMode.LOAD.id());
        assertEquals("start", TrainingMode.START.id());
    }

    @Test
    void theOptionNamesThePluginParameterAndTheConstant() {
        assertEquals("micronaut.docker.jdkAotCache.trainingMode=LOAD", TrainingMode.LOAD.option());
        assertEquals("micronaut.docker.jdkAotCache.trainingMode=START", TrainingMode.START.option());
    }
}
