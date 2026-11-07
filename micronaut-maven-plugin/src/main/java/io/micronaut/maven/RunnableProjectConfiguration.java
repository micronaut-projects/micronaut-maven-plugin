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
package io.micronaut.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.lifecycle.internal.MojoDescriptorCreator;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.classworlds.realm.ClassRealm;
import org.codehaus.plexus.component.configurator.ComponentConfigurationException;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;

/**
 * Configures an aggregator goal, {@code mn:run}, {@code mn:dev} or {@code mn:test}, as the application it selects from a
 * reactor declares it. Maven configured the mojo from the project the goal was invoked on, the reactor root when the
 * goal runs from an aggregator that does not declare the plugin, so a property-backed parameter, such as
 * {@code micronaut.test.resources.enabled}, and the plugin's configuration came from the root.
 *
 * @since 5.1.0
 */
final class RunnableProjectConfiguration {

    private static final String DEFAULT_CLI_EXECUTION = "default-cli";

    private RunnableProjectConfiguration() {
    }

    /**
     * Applies the application's configuration to the goal as Maven would have done had the goal run on it: its
     * plugin-level {@code micronaut-maven-plugin} configuration, with the configuration of the execution selected on the
     * command line, {@code default-cli} unless one is named, merged over it, restricted to the goal's parameters and merged over the goal's defaults and property expressions, is
     * evaluated against the application and applied with Maven's configurator, after the goal's parameters are reset.
     *
     * @param goal the goal, as Maven configured it
     * @param pristine a new instance of the goal, holding the values its parameters start with
     * @param execution the goal's execution
     * @param evaluator what evaluates the configuration against the application, from {@link #evaluator}
     * @param project the application
     * @param configurator Maven's configurator of mojos
     * @throws ComponentConfigurationException if the configuration cannot be applied
     */
    static void apply(Mojo goal,
                      Mojo pristine,
                      MojoExecution execution,
                      PluginParameterExpressionEvaluator evaluator,
                      MavenProject project,
                      ComponentConfigurator configurator) throws ComponentConfigurationException {
        MojoDescriptor descriptor = execution.getMojoDescriptor();
        Xpp3Dom configuration = Xpp3Dom.mergeXpp3Dom(declaredConfiguration(project, descriptor, execution.getExecutionId()), MojoDescriptorCreator.convert(descriptor));
        resetParameters(goal, pristine, descriptor);
        ClassRealm realm = descriptor.getPluginDescriptor().getClassRealm();
        configurator.configureComponent(goal, new XmlPlexusConfiguration(configuration), evaluator, realm);
    }

    /**
     * An evaluator whose expressions resolve against the application, not against the reactor root an aggregator
     * invocation started from.
     *
     * @param session the session
     * @param project the application
     * @param execution the goal's execution
     * @return the evaluator
     */
    static PluginParameterExpressionEvaluator evaluator(MavenSession session, MavenProject project, MojoExecution execution) {
        MavenSession projectSession = session.clone();
        projectSession.setCurrentProject(project);
        return new PluginParameterExpressionEvaluator(projectSession, execution);
    }

    /**
     * The configuration of the goal the project declares: its plugin-level {@code micronaut-maven-plugin} configuration,
     * with the selected execution's merged over it, restricted to the goal's parameters.
     *
     * @param project the project
     * @param descriptor the goal's descriptor
     * @param executionId the id of the execution selected on the command line, such as {@code local} for {@code mn:dev@local}; {@code default-cli} when null
     * @return the configuration, empty when the project declares none
     */
    static Xpp3Dom declaredConfiguration(MavenProject project, MojoDescriptor descriptor, String executionId) {
        Xpp3Dom own = new Xpp3Dom("configuration");
        Plugin plugin = project.getPlugin(THIS_PLUGIN);
        if (plugin == null) {
            return own;
        }
        Xpp3Dom effective = effectiveConfiguration(plugin, executionId == null ? DEFAULT_CLI_EXECUTION : executionId);
        if (effective != null) {
            for (Xpp3Dom child : effective.getChildren()) {
                // another goal's settings, such as the AOT goals' or mn:run's watches, are not this goal's to apply
                if (isParameter(descriptor, child.getName())) {
                    own.addChild(new Xpp3Dom(child));
                }
            }
        }
        return own;
    }

    private static Xpp3Dom effectiveConfiguration(Plugin plugin, String selected) {
        Xpp3Dom effective = plugin.getConfiguration() instanceof Xpp3Dom dom ? new Xpp3Dom(dom) : null;
        for (PluginExecution execution : plugin.getExecutions()) {
            if (selected.equals(execution.getId()) && execution.getConfiguration() instanceof Xpp3Dom executionDom) {
                effective = effective == null ? new Xpp3Dom(executionDom) : Xpp3Dom.mergeXpp3Dom(new Xpp3Dom(executionDom), effective);
            }
        }
        return effective;
    }

    private static boolean isParameter(MojoDescriptor descriptor, String name) {
        if (descriptor.getParameterMap().containsKey(name)) {
            return true;
        }
        return descriptor.getParameters() != null && descriptor.getParameters().stream().anyMatch(parameter -> name.equals(parameter.getAlias()));
    }

    /**
     * Resets the goal's parameters to the values a new mojo starts with, so that a value Maven resolved against the
     * reactor root, which the application's configuration leaves unresolved, does not remain: Maven's configurator
     * does not assign an expression that resolves to nothing.
     */
    private static void resetParameters(Mojo goal, Mojo pristine, MojoDescriptor descriptor) {
        if (descriptor.getParameters() == null) {
            return;
        }
        for (Parameter parameter : descriptor.getParameters()) {
            if (!parameter.isEditable()) {
                // read-only parameters, such as the mojo execution, are the same for every project
                continue;
            }
            Field field = parameterField(goal.getClass(), parameter.getName());
            if (field != null) {
                try {
                    field.set(goal, field.get(pristine));
                } catch (IllegalAccessException e) {
                    throw new IllegalStateException("Cannot reset the parameter " + parameter.getName(), e);
                }
            }
        }
    }

    private static Field parameterField(Class<?> goalType, String name) {
        for (Class<?> type = goalType; type != null && type != Object.class; type = type.getSuperclass()) {
            try {
                Field field = type.getDeclaredField(name);
                if (Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers())) {
                    return null;
                }
                field.setAccessible(true);
                return field;
            } catch (NoSuchFieldException e) {
                // declared by a superclass
            }
        }
        return null;
    }
}
