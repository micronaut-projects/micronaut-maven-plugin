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
package io.micronaut.maven.testresources;

import io.micronaut.maven.RunMojo;
import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.annotations.Component;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.logging.Logger;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomWriter;
import org.twdata.maven.mojoexecutor.MojoExecutor;

import javax.inject.Inject;
import java.io.File;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

import static io.micronaut.maven.RunMojo.THIS_PLUGIN;
import static io.micronaut.maven.testresources.AbstractTestResourcesMojo.CONFIG_PROPERTY_PREFIX;
import static io.micronaut.maven.testresources.StopTestResourcesServerMojo.MICRONAUT_TEST_RESOURCES_KEEPALIVE;

/**
 * A lifecycle extension which determines if the test resources server should
 * be stopped when the build is complete.
 */
@Component(role = AbstractMavenLifecycleParticipant.class, hint = "test-resources")
public class TestResourcesLifecycleExtension extends AbstractMavenLifecycleParticipant  {

    private static final String EXPLICIT_START_SERVICE_GOAL_NAME = "mn:" + StartTestResourcesServerMojo.NAME;
    private static final String EXPLICIT_STOP_SERVICE_GOAL_NAME = "mn:" + StopTestResourcesServerMojo.NAME;

    private final Map<MavenProject, ExpressionEvaluator> perProjectEvaluator = new ConcurrentHashMap<>();
    private final Map<MavenProject, TestResourcesConfiguration> perProjectConfiguration = new ConcurrentHashMap<>();

    private final Logger logger;

    @Inject
    @SuppressWarnings("CdiInjectionPointsInspection")
    public TestResourcesLifecycleExtension(Logger logger) {
        this.logger = logger;
    }

    @Override
    public void afterProjectsRead(MavenSession session) {
        session.getAllProjects().forEach(currentProject -> {
            Build build = currentProject.getBuild();
            withPlugin(build, plugin -> {
                ExpressionEvaluator evaluator = perProjectEvaluator.computeIfAbsent(currentProject, p -> initEvaluator(p, session));
                TestResourcesConfiguration configuration = perProjectConfiguration.computeIfAbsent(currentProject, mavenProject -> initConfiguration(plugin));

                boolean enabled = isEnabled(evaluator, configuration);
                if (enabled) {
                    List<String> goals = session.getGoals();
                    if (goals.stream().anyMatch(EXPLICIT_START_SERVICE_GOAL_NAME::equals)) {
                        // we need to keep the server alive at the end of the build

                        Xpp3Dom flag = new Xpp3Dom(MICRONAUT_TEST_RESOURCES_KEEPALIVE);
                        Xpp3Dom pluginConfiguration = (Xpp3Dom) plugin.getConfiguration();
                        pluginConfiguration.addChild(flag);
                        flag.setValue("true");
                    }
                }
            });
        });
    }

    @Override
    public void afterSessionEnd(MavenSession session) {
        if (session.getGoals().stream().noneMatch(s -> s.equals(EXPLICIT_START_SERVICE_GOAL_NAME) || s.equals(EXPLICIT_STOP_SERVICE_GOAL_NAME))) {
            session.getAllProjects().forEach(currentProject -> {
                Build build = currentProject.getBuild();
                withPlugin(build, plugin -> {
                    ExpressionEvaluator evaluator = perProjectEvaluator.computeIfAbsent(currentProject, p -> initEvaluator(p, session));
                    TestResourcesConfiguration configuration = perProjectConfiguration.computeIfAbsent(currentProject, mavenProject -> initConfiguration(plugin));
                    boolean enabled = isEnabled(evaluator, configuration);
                    boolean shared = isShared(evaluator, configuration);
                    File buildDirectory = new File(build.getDirectory());

                    TestResourcesHelper helper = new TestResourcesHelper(session, enabled, shared, buildDirectory);
                    if (shared) {
                        String sharedServerNamespace = findSharedServerNamespace(evaluator, configuration);
                        helper.setSharedServerNamespace(sharedServerNamespace);
                    }
                    try {
                        helper.stop();
                    } catch (Exception e) {
                        logger.error(e.getMessage(), e);
                    }
                });
            });
        }
    }

    private String findSharedServerNamespace(ExpressionEvaluator evaluator, TestResourcesConfiguration configuration) {
        try {
            String result = (String) evaluator.evaluate("${" + CONFIG_PROPERTY_PREFIX + "namespace" + "}");
            if (result != null) {
                return result;
            } else if (configuration != null) {
                return configuration.getSharedServerNamespace();
            }
        } catch (ExpressionEvaluationException e) {
            return null;
        }
        return null;
    }

    private boolean isShared(ExpressionEvaluator evaluator, TestResourcesConfiguration configuration) {
        Boolean result = evaluateBooleanProperty(evaluator, CONFIG_PROPERTY_PREFIX + "shared");
        if (result != null) {
            return result;
        } else if (configuration != null) {
            return configuration.isShared();
        }
        return false;
    }

    private boolean isEnabled(ExpressionEvaluator evaluator, TestResourcesConfiguration configuration) {
        Boolean result = evaluateBooleanProperty(evaluator, CONFIG_PROPERTY_PREFIX + "enabled");
        if (result != null) {
            return result;
        } else if (configuration != null) {
            return configuration.isTestResourcesEnabled();
        }
        return false;
    }

    private TestResourcesConfiguration initConfiguration(Plugin plugin) {
        Xpp3Dom configuration = (Xpp3Dom) plugin.getConfiguration();
        if (configuration == null) {
            configuration = MojoExecutor.configuration();
            plugin.setConfiguration(configuration);
        }
        Writer writer = new StringWriter();
        Xpp3DomWriter.write(writer, configuration);
        return parseConfiguration(configuration);
    }

    private TestResourcesConfiguration parseConfiguration(Xpp3Dom dom) {
        TestResourcesConfiguration config = null;
        if (dom != null) {
            config = new TestResourcesConfiguration();

            Xpp3Dom testResourcesEnabled = dom.getChild("testResourcesEnabled");
            if (testResourcesEnabled != null) {
                config.setTestResourcesEnabled(Boolean.parseBoolean(testResourcesEnabled.getValue()));
            }

            Xpp3Dom shared = dom.getChild("shared");
            if (shared != null) {
                config.setShared(Boolean.parseBoolean(shared.getValue()));
            }

            Xpp3Dom sharedServerNamespace = dom.getChild("sharedServerNamespace");
            if (sharedServerNamespace != null) {
                config.setSharedServerNamespace(sharedServerNamespace.getValue());
            }

            Xpp3Dom debugServer = dom.getChild("debugServer");
            if (debugServer != null) {
                config.setShared(Boolean.parseBoolean(debugServer.getValue()));
            }
        }
        return config;
    }

    private Boolean evaluateBooleanProperty(ExpressionEvaluator evaluator, String property) {
        try {
            Object result = evaluator.evaluate("${" + property + "}");
            if (result instanceof Boolean b) {
                return b;
            } else if (result instanceof String s && (s.equals(Boolean.TRUE.toString()) || s.equals(Boolean.FALSE.toString()))) {
                return Boolean.parseBoolean(s);
            }
        } catch (ExpressionEvaluationException e) {
            return false;
        }
        return null;
    }

    private ExpressionEvaluator initEvaluator(MavenProject currentProject, MavenSession session) {
        Plugin thisPlugin = currentProject.getPlugin(THIS_PLUGIN);
        MojoExecution execution = new MojoExecution(thisPlugin, null, null);
        MavenProject actualCurrentProject = session.getCurrentProject();
        ExpressionEvaluator evaluator;

        // Maven 3: PluginParameterExpressionEvaluator gets the current project from the session:
        // synchronize in case another thread wants to fetch the real current project in between
        synchronized (perProjectEvaluator) {
            session.setCurrentProject(currentProject);
            evaluator = new PluginParameterExpressionEvaluator(session, execution);
            session.setCurrentProject(actualCurrentProject);
        }

        return evaluator;
    }

    private static void withPlugin(Build build, Consumer<? super Plugin> consumer) {
        build.getPlugins()
                .stream()
                .filter(p -> RunMojo.THIS_PLUGIN.equals(p.getGroupId() + ":" + p.getArtifactId()))
                .findFirst()
                .ifPresent(consumer);
    }
}
