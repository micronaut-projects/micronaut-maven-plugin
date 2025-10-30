/*
 * Copyright 2017-2025 original authors
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
package io.micronaut.maven;

import io.micronaut.maven.InvocationResultWithOutput.PerGoalOutputHandler;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link PerGoalOutputHandler}.
 */
class PerGoalOutputHandlerTest {

    @Test
    void parsesGoalsAndAggregatesOutput() throws Exception {
        PerGoalOutputHandler handler = new PerGoalOutputHandler();
        feedLines(handler);

        // First goal
        var output = handler.getOutput("enforcer:enforce");
        assertEquals(8, output.size());
        // First module
        assertEquals("--- enforcer:3.6.2:enforce (default) @ micronaut-maven-plugin-parent ---",  output.get(0));
        assertEquals("Rule 0: org.apache.maven.enforcer.rules.version.RequireMavenVersion passed",  output.get(1));
        assertEquals("Rule 1: org.apache.maven.enforcer.rules.version.RequireJavaVersion passed",  output.get(2));

        // Rest of the modules have an empty output
        assertEquals("--- enforcer:3.6.2:enforce (default) @ micronaut-maven-core ---",  output.get(3));
        assertEquals("--- enforcer:3.6.2:enforce (default) @ micronaut-maven-enforcer-rules ---",  output.get(4));

        // Goal in the middle
        output = handler.getOutput("jacoco:prepare-agent");
        assertEquals(12, output.size());
        assertEquals("--- jacoco:0.8.14:prepare-agent (prepare-agent) @ micronaut-maven-plugin-parent ---", output.get(0));
        assertTrue(output.get(1).startsWith("argLine set to"));
        assertTrue(output.get(11).startsWith("invoker.mavenOpts set to"));

        // Last goal
        output = handler.getOutput("surefire:test");
        assertEquals(77, output.size());
        assertEquals("No tests to run.", output.get(76));
    }

    private static void feedLines(PerGoalOutputHandler handler) throws IOException {
        var lines = Files.readAllLines(Path.of("src/test/resources/mvn-output-sample.txt"), StandardCharsets.UTF_8);
        for (String line : lines) {
            handler.consumeLine(line);
        }

    }

}
