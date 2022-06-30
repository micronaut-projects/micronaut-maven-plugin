/*
 * Copyright 2017-2021 original authors
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
package io.micronaut.build.testresources;

import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.codehaus.plexus.component.annotations.Component;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import java.util.List;
import java.util.function.Consumer;

import static io.micronaut.build.testresources.MicronautStopTestResourcesServerMojo.MICRONAUT_TEST_RESOURCES_KEEPALIVE;

/**
 * A lifecycle extension which determines if the test resources server should
 * be stopped when the build is complete.
 */
@Component(role = AbstractMavenLifecycleParticipant.class, hint = "test-resources")
public class TestResourcesLifecycleExtension extends AbstractMavenLifecycleParticipant {

    private static final String EXPLICIT_START_SERVICE_GOAL_NAME = "mn:" + MicronautStartTestResourcesServerMojo.NAME;

    @Override
    public void afterProjectsRead(MavenSession session) {
        List<String> goals = session.getGoals();
        if (goals.stream().anyMatch(EXPLICIT_START_SERVICE_GOAL_NAME::equals)) {
            // we need to keep the server alive at the end of the build
            session.getAllProjects().forEach(p -> {
                Build build = p.getBuild();
                withPlugin(build, "micronaut-maven-plugin", plugin -> {
                    Xpp3Dom configuration = (Xpp3Dom) plugin.getConfiguration();
                    if (configuration == null) {
                        configuration = new Xpp3Dom("configuration");
                        plugin.setConfiguration(configuration);
                    }
                    Xpp3Dom flag = new Xpp3Dom(MICRONAUT_TEST_RESOURCES_KEEPALIVE);
                    configuration.addChild(flag);
                    flag.setValue("true");
                });
            });
        }
    }

    private static void withPlugin(Build build, String artifactId, Consumer<? super Plugin> consumer) {
        build.getPlugins()
                .stream()
                .filter(p -> artifactId.equals(p.getArtifactId()))
                .findFirst()
                .ifPresent(consumer);
    }
}
