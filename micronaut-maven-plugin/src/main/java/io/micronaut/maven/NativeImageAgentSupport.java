/*
 * Copyright 2017-2022 original authors
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
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.graalvm.buildtools.agent.AgentConfiguration;
import org.graalvm.buildtools.agent.StandardAgentMode;
import org.graalvm.buildtools.utils.SharedConstants;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

final class NativeImageAgentSupport {

    static final String AGENT_PROPERTY = "agent";
    static final String IMAGECODE_PROPERTY = "org.graalvm.nativeimage.imagecode";
    static final String NATIVE_IMAGE_AGENTLIB = "-agentlib:native-image-agent";
    static final String NATIVE_IMAGE_IMAGECODE = "-Dorg.graalvm.nativeimage.imagecode=agent";
    static final String NATIVE_MAVEN_PLUGIN = DockerfileMojo.NATIVE_BUILD_TOOLS_MAVEN_PLUGIN;

    private NativeImageAgentSupport() {
    }

    static List<String> computeJvmArguments(MavenSession session,
                                            MavenProject project,
                                            File targetDirectory,
                                            List<String> existingJvmArguments) throws MojoExecutionException {
        AgentConfiguration configuration = resolveAgentConfiguration(session, project);
        if (!configuration.isEnabled()) {
            return List.of();
        }
        if (containsNativeImageAgent(existingJvmArguments)) {
            throw new MojoExecutionException("Native image agent support is enabled through native-build-tools, so mn.jvmArgs must not define -agentlib:native-image-agent manually");
        }
        if (containsNativeImageImagecode(session, existingJvmArguments)) {
            throw new MojoExecutionException("Native image agent support is enabled through native-build-tools, so mn.jvmArgs and Maven properties must not define org.graalvm.nativeimage.imagecode manually");
        }
        String outputDirectory = new File(targetDirectory, SharedConstants.AGENT_OUTPUT_FOLDER + File.separator + "main").getAbsolutePath();
        String agentArgument = NATIVE_IMAGE_AGENTLIB + "=" + configuration.getAgentCommandLine().stream()
            .map(option -> option.replace(SharedConstants.AGENT_OUTPUT_DIRECTORY_MARKER, outputDirectory))
            .reduce((left, right) -> left + "," + right)
            .orElse("");
        return List.of(agentArgument, NATIVE_IMAGE_IMAGECODE);
    }

    private static AgentConfiguration resolveAgentConfiguration(MavenSession session, MavenProject project) throws MojoExecutionException {
        Plugin plugin = project.getPlugin(NATIVE_MAVEN_PLUGIN);
        Xpp3Dom configurationRoot = plugin != null && plugin.getConfiguration() instanceof Xpp3Dom dom ? dom : null;
        Xpp3Dom agentNode = child(configurationRoot, AGENT_PROPERTY);
        Optional<Boolean> commandLineOverride = readAgentOverride(session);
        if (commandLineOverride.isPresent()) {
            if (!commandLineOverride.get()) {
                return new AgentConfiguration();
            }
            return agentNode == null ? new AgentConfiguration(new StandardAgentMode()) : parseAgentConfiguration(project, agentNode);
        }
        if (!isEnabledInPom(agentNode)) {
            return new AgentConfiguration();
        }
        return parseAgentConfiguration(project, agentNode);
    }

    private static AgentConfiguration parseAgentConfiguration(MavenProject project, Xpp3Dom agentNode) throws MojoExecutionException {
        String mode = value(agentNode, "defaultMode").orElse("standard");
        if (!"standard".equalsIgnoreCase(mode)) {
            throw new MojoExecutionException("mn:run supports native-image agent configuration only in standard mode. Configured mode [" + mode + "] must use the upstream native-maven-plugin exec workflow instead");
        }
        Xpp3Dom options = child(agentNode, "options");
        return new AgentConfiguration(
            filterFiles(project, options, "callerFilterFiles"),
            filterFiles(project, options, "accessFilterFiles"),
            parseBoolean(options, "builtinCallerFilter").orElse(null),
            parseBoolean(options, "builtinHeuristicFilter").orElse(null),
            parseBoolean(options, "enableExperimentalPredefinedClasses").orElse(null),
            parseBoolean(options, "enableExperimentalUnsafeAllocationTracing").orElse(null),
            parseBoolean(options, "trackReflectionMetadata").orElse(null),
            new StandardAgentMode()
        );
    }

    private static ArrayList<String> filterFiles(MavenProject project, Xpp3Dom options, String name) {
        ArrayList<String> files = new ArrayList<>();
        Xpp3Dom parent = child(options, name);
        if (parent == null) {
            return files;
        }
        for (Xpp3Dom filterFile : parent.getChildren("filterFile")) {
            String value = filterFile.getValue();
            if (value != null && !value.isBlank()) {
                File file = new File(value);
                files.add(file.isAbsolute() ? file.getPath() : new File(project.getBasedir(), value).getAbsolutePath());
            }
        }
        return files;
    }

    private static boolean isEnabledInPom(Xpp3Dom agentNode) throws MojoExecutionException {
        if (agentNode == null) {
            return false;
        }
        return parseBoolean(agentNode, "enabled").orElse(false);
    }

    private static Optional<Boolean> readAgentOverride(MavenSession session) throws MojoExecutionException {
        Optional<String> value = readProperty(session.getUserProperties(), AGENT_PROPERTY)
            .or(() -> readProperty(session.getSystemProperties(), AGENT_PROPERTY));
        if (value.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(parseBoolean(AGENT_PROPERTY, value.get()));
    }

    private static Optional<String> readProperty(Properties properties, String key) {
        if (properties == null) {
            return Optional.empty();
        }
        return Optional.ofNullable(properties.getProperty(key));
    }

    private static Optional<Boolean> parseBoolean(Xpp3Dom parent, String childName) throws MojoExecutionException {
        Xpp3Dom child = child(parent, childName);
        if (child == null || child.getValue() == null) {
            return Optional.empty();
        }
        return Optional.of(parseBoolean("<" + childName + ">", child.getValue()));
    }

    private static Boolean parseBoolean(String name, String value) throws MojoExecutionException {
        if ("true".equalsIgnoreCase(value)) {
            return true;
        }
        if ("false".equalsIgnoreCase(value)) {
            return false;
        }
        throw new MojoExecutionException("Invalid boolean value [" + value + "] for " + name + ". Use true or false");
    }

    private static Optional<String> value(Xpp3Dom parent, String childName) {
        Xpp3Dom child = child(parent, childName);
        return child == null ? Optional.empty() : Optional.ofNullable(child.getValue());
    }

    private static Xpp3Dom child(Xpp3Dom parent, String childName) {
        return parent == null ? null : parent.getChild(childName);
    }

    private static boolean containsNativeImageAgent(List<String> jvmArguments) {
        return jvmArguments.stream().anyMatch(argument -> argument.startsWith(NATIVE_IMAGE_AGENTLIB));
    }

    private static boolean containsNativeImageImagecode(MavenSession session, List<String> jvmArguments) {
        return jvmArguments.stream().anyMatch(NativeImageAgentSupport::isNativeImageImagecodeArgument)
            || hasProperty(session.getUserProperties(), IMAGECODE_PROPERTY)
            || hasProperty(session.getSystemProperties(), IMAGECODE_PROPERTY);
    }

    private static boolean isNativeImageImagecodeArgument(String argument) {
        return argument.startsWith("-D" + IMAGECODE_PROPERTY + "=");
    }

    private static boolean hasProperty(Properties properties, String key) {
        return properties != null && properties.containsKey(key);
    }
}
