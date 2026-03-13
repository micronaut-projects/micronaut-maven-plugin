/*
 * Copyright 2017-2023 original authors
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
package io.micronaut.maven.compat;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Parent;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.OptionalInt;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MicronautMajorVersionCheckerTest {

    private final MicronautMajorVersionChecker checker = new MicronautMajorVersionChecker();

    @Test
    void analyzesLifecycleCoordinatesAgainstBaseline() {
        Parent parent = new Parent();
        parent.setGroupId("io.micronaut.platform");
        parent.setArtifactId("micronaut-parent");
        parent.setVersion("5.0.0-SNAPSHOT");

        var coordinates = checker.collectLifecycleCoordinates(parent, List.of(
            dependency("io.micronaut.starter", "micronaut-starter-aws-cdk", "4.10.9")
        ), List.of());

        var analysis = checker.analyzeForLifecycle(coordinates);
        assertEquals(OptionalInt.of(5), analysis.baselineMajor());
        assertEquals(1, analysis.conflicts().size());
        assertTrue(analysis.hasConflict());
    }

    @Test
    void ignoresSiblingMicronautRepositoryMajorsForEnforcerCoordinates() {
        var coordinates = checker.collectEnforcerCoordinates(
            List.of(dependency("io.micronaut", "micronaut-inject", "5.0.0")),
            List.of(dependency("io.micronaut.validation", "micronaut-validation", "4.9.1")),
            List.of()
        );

        assertFalse(checker.hasMixedMajors(coordinates));
    }

    @Test
    void detectsMixedMajorsWithinIoMicronautFamily() {
        var coordinates = checker.collectEnforcerCoordinates(
            List.of(
                dependency("io.micronaut", "micronaut-inject", "5.0.0"),
                dependency("io.micronaut", "micronaut-runtime", "4.9.1")
            ),
            List.of(),
            List.of()
        );

        assertTrue(checker.hasMixedMajors(coordinates));
        assertFalse(checker.buildEnforcerFailureMessage(coordinates).isBlank());
    }

    private Dependency dependency(String groupId, String artifactId, String version) {
        Dependency dependency = new Dependency();
        dependency.setGroupId(groupId);
        dependency.setArtifactId(artifactId);
        dependency.setVersion(version);
        return dependency;
    }
}
