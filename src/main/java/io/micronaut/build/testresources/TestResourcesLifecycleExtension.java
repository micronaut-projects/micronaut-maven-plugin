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

import io.micronaut.build.RunMojo;
import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.MavenExecutionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.monitor.logging.DefaultLog;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.logging.console.ConsoleLogger;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import java.io.File;
import java.util.List;
import java.util.Properties;
import java.util.function.Consumer;

import static io.micronaut.build.testresources.AbstractTestResourcesMojo.CONFIG_PROPERTY_PREFIX;
import static io.micronaut.build.testresources.StopTestResourcesServerMojo.MICRONAUT_TEST_RESOURCES_KEEPALIVE;

/**
 * A lifecycle extension which determines if the test resources server should
 * be stopped when the build is complete.
 */
public class TestResourcesLifecycleExtension extends AbstractMavenLifecycleParticipant {

    private static final String EXPLICIT_START_SERVICE_GOAL_NAME = "mn:" + StartTestResourcesServerMojo.NAME;

    @Override
    public void afterProjectsRead(MavenSession session) {
        List<String> goals = session.getGoals();
        if (goals.stream().anyMatch(EXPLICIT_START_SERVICE_GOAL_NAME::equals)) {
            // we need to keep the server alive at the end of the build
            session.getAllProjects().forEach(p -> {
                Build build = p.getBuild();
                withPlugin(build, "micronaut-maven-plugin", plugin -> {
                    Xpp3Dom configuration = (Xpp3Dom) plugin.getConfiguration();
                    boolean enabled = isEnabled(configuration, p);
                    if (enabled) {
                        if (configuration == null) {
                            configuration = new Xpp3Dom("configuration");
                            plugin.setConfiguration(configuration);
                        }
                        Xpp3Dom flag = new Xpp3Dom(MICRONAUT_TEST_RESOURCES_KEEPALIVE);
                        configuration.addChild(flag);
                        flag.setValue("true");
                    }
                });
            });
        }
    }

    @Override
    public void afterSessionEnd(MavenSession session) throws MavenExecutionException {
        if (session.getGoals().stream().noneMatch(EXPLICIT_START_SERVICE_GOAL_NAME::equals)) {
            MavenProject project = session.getAllProjects().stream()
                    .filter(p -> p.getPlugin(RunMojo.THIS_PLUGIN) != null)
                    .findFirst()
                    .orElseThrow(() -> new MavenExecutionException("Could not find plugin" + RunMojo.THIS_PLUGIN, (Throwable) null));

            Xpp3Dom configuration = (Xpp3Dom) project.getPlugin(RunMojo.THIS_PLUGIN).getConfiguration();

            boolean enabled = isEnabled(configuration, project);
            boolean keepAlive = isKeepAlive(configuration, project);
            boolean shared = isShared(configuration, project);
            Log log = new DefaultLog(new ConsoleLogger());
            File buildDirectory = new File(project.getBuild().getDirectory());

            StopTestResourcesHelper service = new StopTestResourcesHelper(enabled, keepAlive, shared, log, buildDirectory);
            try {
                service.stopTestResources();
            } catch (Exception e) {
                //no op
            }
        }
    }

    private boolean isShared(Xpp3Dom configuration, MavenProject project) {
        return evaluateBooleanProperty(project.getProperties(), configuration, CONFIG_PROPERTY_PREFIX + "shared", "shared");
    }

    private boolean isKeepAlive(Xpp3Dom configuration, MavenProject project) {
        return evaluateBooleanProperty(project.getProperties(), configuration, MICRONAUT_TEST_RESOURCES_KEEPALIVE, "keepAlive");
    }

    private boolean isEnabled(Xpp3Dom configuration, MavenProject project) {
        return evaluateBooleanProperty(project.getProperties(), configuration, CONFIG_PROPERTY_PREFIX + "enabled", "testResourcesEnabled");
    }

    private boolean evaluateBooleanProperty(Properties properties, Xpp3Dom configuration, String property, String xmlTag) {
        boolean systemPropertyEnabled = Boolean.getBoolean(property);
        if (systemPropertyEnabled) {
            return true;
        } else {
            boolean projectPropertyEnabled = Boolean.parseBoolean(properties.getProperty(property, "false"));
            if (projectPropertyEnabled) {
                return true;
            } else if (configuration != null) {
                Xpp3Dom testResourcesEnabled = configuration.getChild(xmlTag);
                if (testResourcesEnabled != null && testResourcesEnabled.getValue() != null) {
                    return Boolean.parseBoolean(testResourcesEnabled.getValue());
                }
            }
        }
        return false;
    }

    private static void withPlugin(Build build, String artifactId, Consumer<? super Plugin> consumer) {
        build.getPlugins()
                .stream()
                .filter(p -> artifactId.equals(p.getArtifactId()))
                .findFirst()
                .ifPresent(consumer);
    }
}
