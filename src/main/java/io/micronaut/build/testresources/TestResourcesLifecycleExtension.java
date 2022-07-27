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
import org.apache.maven.MavenExecutionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.monitor.logging.DefaultLog;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.logging.console.ConsoleLogger;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.util.artifact.JavaScopes;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

import static io.micronaut.build.RunMojo.THIS_PLUGIN;
import static io.micronaut.build.services.DependencyResolutionService.TEST_RESOURCES_ARTIFACT_ID_PREFIX;
import static io.micronaut.build.services.DependencyResolutionService.TEST_RESOURCES_GROUP;
import static io.micronaut.build.testresources.AbstractTestResourcesMojo.CONFIG_PROPERTY_PREFIX;
import static io.micronaut.build.testresources.StopTestResourcesServerMojo.MICRONAUT_TEST_RESOURCES_KEEPALIVE;

/**
 * A lifecycle extension which determines if the test resources server should
 * be stopped when the build is complete.
 */
public class TestResourcesLifecycleExtension extends AbstractMavenLifecycleParticipant {

    private static final String EXPLICIT_START_SERVICE_GOAL_NAME = "mn:" + StartTestResourcesServerMojo.NAME;
    private static final String EXPLICIT_STOP_SERVICE_GOAL_NAME = "mn:" + StopTestResourcesServerMojo.NAME;

    private final Object sessionLock = new Object();

    private ExpressionEvaluator evaluator;

    @Override
    public void afterProjectsRead(MavenSession session) {
        session.getAllProjects().forEach(p -> {
            Build build = p.getBuild();
            withPlugin(build, THIS_PLUGIN, plugin -> {
                Xpp3Dom configuration = (Xpp3Dom) plugin.getConfiguration();
                boolean enabled = isEnabled(session);
                if (enabled) {
                    if (configuration == null) {
                        configuration = new Xpp3Dom("configuration");
                        plugin.setConfiguration(configuration);
                    }
                    List<String> goals = session.getGoals();
                    if (goals.stream().anyMatch(EXPLICIT_START_SERVICE_GOAL_NAME::equals)) {
                        // we need to keep the server alive at the end of the build

                        Xpp3Dom flag = new Xpp3Dom(MICRONAUT_TEST_RESOURCES_KEEPALIVE);
                        configuration.addChild(flag);
                        flag.setValue("true");
                    }
                    Dependency clientDependency = new Dependency();
                    clientDependency.setGroupId(TEST_RESOURCES_GROUP);
                    clientDependency.setArtifactId(TEST_RESOURCES_ARTIFACT_ID_PREFIX + "client");
                    clientDependency.setVersion(String.valueOf(p.getProperties().get(CONFIG_PROPERTY_PREFIX + "version")));
                    clientDependency.setScope(JavaScopes.TEST);
                    p.getDependencies().add(clientDependency);
                }
            });
        });
    }

    @Override
    public void afterSessionEnd(MavenSession session) throws MavenExecutionException {
        if (session.getGoals().stream().noneMatch(s -> s.equals(EXPLICIT_START_SERVICE_GOAL_NAME) || s.equals(EXPLICIT_STOP_SERVICE_GOAL_NAME))) {
            MavenProject project = findProjectWithThisPlugin(session)
                    .orElseThrow(() -> new MavenExecutionException("Could not find plugin" + THIS_PLUGIN, (Throwable) null));

            boolean enabled = isEnabled(session);
            boolean keepAlive = isKeepAlive(session);
            boolean shared = isShared(session);
            Log log = new DefaultLog(new ConsoleLogger());
            File buildDirectory = new File(project.getBuild().getDirectory());

            StopTestResourcesHelper helper = new StopTestResourcesHelper(enabled, keepAlive, shared, log, buildDirectory);
            try {
                helper.stopTestResources();
            } catch (Exception e) {
                //no op
            }
        }
    }

    private boolean isShared(MavenSession session) {
        return evaluateBooleanProperty(session, CONFIG_PROPERTY_PREFIX + "shared");
    }

    private boolean isKeepAlive(MavenSession session) {
        return evaluateBooleanProperty(session, MICRONAUT_TEST_RESOURCES_KEEPALIVE);
    }

    private boolean isEnabled(MavenSession session) {
        return evaluateBooleanProperty(session, CONFIG_PROPERTY_PREFIX + "enabled");
    }

    private boolean evaluateBooleanProperty(MavenSession session, String property) {
        try {
            Object result = getEvaluator(session).evaluate("${" + property + "}");
            if (result instanceof Boolean) {
                return (Boolean) result;
            } else if (result instanceof String && (result.equals(Boolean.TRUE.toString()) || result.equals(Boolean.FALSE.toString()))) {
                return Boolean.parseBoolean((String) result);
            }
        } catch (ExpressionEvaluationException e) {
            return false;
        }
        return false;
    }

    private ExpressionEvaluator getEvaluator(MavenSession session) {
        if (evaluator == null) {
            findProjectWithThisPlugin(session).ifPresent(projectWithThisPlugin -> {
                Plugin thisPlugin = projectWithThisPlugin.getPlugin(THIS_PLUGIN);
                MojoExecution execution = new MojoExecution(thisPlugin, null, null);
                MavenProject currentProject = session.getCurrentProject();

                // Maven 3: PluginParameterExpressionEvaluator gets the current project from the session:
                // synchronize in case another thread wants to fetch the real current project in between
                synchronized (sessionLock) {
                    session.setCurrentProject(projectWithThisPlugin);
                    evaluator = new PluginParameterExpressionEvaluator(session, execution);
                    session.setCurrentProject(currentProject);
                }
            });
        }

        return evaluator;
    }

    private static Optional<MavenProject> findProjectWithThisPlugin(MavenSession session) {
        return session.getAllProjects().stream()
                .filter(p -> p.getPlugin(THIS_PLUGIN) != null)
                .findFirst();
    }

    private static void withPlugin(Build build, String groupIdArtifactId, Consumer<? super Plugin> consumer) {
        build.getPlugins()
                .stream()
                .filter(p -> groupIdArtifactId.equals(p.getGroupId() + ":" + p.getArtifactId()))
                .findFirst()
                .ifPresent(consumer);
    }
}
