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
package io.micronaut.maven.dev;

import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * The manifest {@code io.micronaut.dev.MicronautDevMain} reads, built from the Maven model: the
 * classpaths, the roots, the compiler options as the compiler plugin is configured, and the settings
 * of the {@code mn:dev} goal. Written as a properties file beside argument files holding the long
 * classpaths, under {@code target/micronaut-dev}.
 *
 * @since 5.1.0
 */
public final class DevManifest {

    /**
     * The name of the manifest file.
     */
    public static final String FILE_NAME = "dev.properties";

    /**
     * The name of the trigger file the launcher watches in build-tool mode.
     */
    public static final String TRIGGER_FILE_NAME = "reload";

    /**
     * The name of the directory under the build directory holding the manifest.
     */
    public static final String DIRECTORY_NAME = "micronaut-dev";

    private static final String PREFIX = "micronaut.dev.";
    private static final String COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    private static final String DEFAULT_COMPILE_EXECUTION = "default-compile";
    private static final String GENERATED_SOURCES = "generated-sources/annotations";

    private final Map<String, String> entries = new LinkedHashMap<>();
    private final Map<String, List<String>> argumentFiles = new LinkedHashMap<>();

    private DevManifest() {
    }

    /**
     * Builds the manifest of a project.
     *
     * @param project the runnable project
     * @param reactor the reactor projects the runnable one depends on, itself included, whose outputs are reloadable
     * @param settings the goal's settings
     * @param runtimeClasspath the runtime classpath: the modules, never a reactor project's output
     * @param compileClasspath the compile classpath, the modules only
     * @param processorPath the annotation processor path
     * @param evaluator what evaluates the expressions of the compiler plugin's configuration
     * @return the manifest
     */
    public static DevManifest of(MavenProject project,
                                 List<MavenProject> reactor,
                                 Settings settings,
                                 List<File> runtimeClasspath,
                                 List<File> compileClasspath,
                                 List<File> processorPath,
                                 ExpressionEvaluator evaluator) {
        DevManifest manifest = new DevManifest();
        Path build = Path.of(project.getBuild().getDirectory());
        manifest.entries.put(PREFIX + "main-class", settings.mainClass());
        manifest.entries.put(PREFIX + "project-dir", project.getBasedir().getAbsolutePath());
        manifest.entries.put(PREFIX + "strategy", settings.strategy());
        manifest.entries.put(PREFIX + "runtime-classpath", manifest.argumentFile("runtime.argfile", paths(runtimeClasspath)));
        // the runnable project's output first: a dependency's source compiles into it, and the generation reads the
        // roots in order, so the new class is found before the dependency's own copy
        List<String> reloadable = new ArrayList<>();
        reloadable.add(project.getBuild().getOutputDirectory());
        for (MavenProject reactorProject : reactor) {
            if (!reloadable.contains(reactorProject.getBuild().getOutputDirectory())) {
                reloadable.add(reactorProject.getBuild().getOutputDirectory());
            }
        }
        manifest.entries.put(PREFIX + "reloadable", String.join(File.pathSeparator, reloadable));
        manifest.entries.put(PREFIX + "compile-classpath", manifest.argumentFile("compile.argfile", paths(compileClasspath)));
        manifest.entries.put(PREFIX + "processor-path", manifest.argumentFile("processors.argfile", paths(processorPath)));
        // the sources of every reloadable project, the runnable one and the reactor projects it depends on: a
        // change in a dependency compiles into the runnable project's output, which the generation reads first
        List<String> javaSources = new ArrayList<>();
        List<String> groovySources = new ArrayList<>();
        List<String> resources = new ArrayList<>();
        for (MavenProject reactorProject : reactor) {
            javaSources.addAll(existing(userSourceRoots(reactorProject)));
            groovySources.addAll(existing(List.of(groovyRoot(reactorProject))));
            resources.addAll(liveResourceRoots(reactorProject));
        }
        if (!javaSources.isEmpty()) {
            manifest.entries.put(PREFIX + "sources.java", String.join(File.pathSeparator, javaSources));
        }
        if (!groovySources.isEmpty()) {
            manifest.entries.put(PREFIX + "sources.groovy", String.join(File.pathSeparator, groovySources));
        }
        if (!resources.isEmpty()) {
            manifest.entries.put(PREFIX + "resources.config", String.join(File.pathSeparator, resources));
            putResourceKind(manifest.entries, "views", resources, "views");
            putResourceKind(manifest.entries, "static", resources, "static", "public");
            putResourceKind(manifest.entries, "i18n", resources, "i18n");
        }
        manifest.entries.put(PREFIX + "compile.mode", settings.compile());
        manifest.entries.put(PREFIX + "compile.incremental", String.valueOf(settings.incremental()));
        String output = project.getBuild().getOutputDirectory();
        if (!javaSources.isEmpty()) {
            manifest.entries.put(PREFIX + "compile.java.output", output);
            manifest.entries.put(PREFIX + "compile.java.generated-sources", build.resolve(GENERATED_SOURCES).toString());
            List<String> options = javacOptions(project, evaluator);
            if (!options.isEmpty()) {
                manifest.entries.put(PREFIX + "compile.java.options", manifest.argumentFile("java-options.argfile", options));
            }
        }
        if (!groovySources.isEmpty()) {
            manifest.entries.put(PREFIX + "compile.groovy.output", output);
        }
        manifest.entries.put(PREFIX + "build-tool", "maven");
        manifest.entries.put(PREFIX + "build-tool.trigger", build.resolve(DIRECTORY_NAME).resolve(TRIGGER_FILE_NAME).toString());
        if (!settings.retain().isEmpty()) {
            manifest.entries.put(PREFIX + "retain", String.join(",", settings.retain()));
        }
        manifest.entries.put(PREFIX + "livereload.port", String.valueOf(settings.liveReloadPort()));
        manifest.entries.put(PREFIX + "livereload.inject-script", String.valueOf(settings.liveReloadInjectScript()));
        return manifest;
    }

    /**
     * @return the entries of the manifest
     */
    public Map<String, String> entries() {
        return Map.copyOf(entries);
    }

    /**
     * Writes the manifest and its argument files.
     *
     * @param directory the directory, {@code target/micronaut-dev}
     * @return the manifest file
     * @throws IOException if a file cannot be written
     */
    public Path writeTo(Path directory) throws IOException {
        Files.createDirectories(directory);
        for (Map.Entry<String, List<String>> argumentFile : argumentFiles.entrySet()) {
            Files.write(directory.resolve(argumentFile.getKey()), argumentFile.getValue(), StandardCharsets.UTF_8);
        }
        Properties properties = new Properties();
        properties.putAll(entries);
        Path file = directory.resolve(FILE_NAME);
        try (OutputStream out = Files.newOutputStream(file)) {
            properties.store(out, "Written by the Micronaut Maven Plugin for io.micronaut.dev.MicronautDevMain");
        }
        return file;
    }

    /**
     * The annotation processor path the compiler plugin is configured with: every {@code <path>} of
     * {@code annotationProcessorPaths}, as artifacts to resolve, versioned by the dependency management
     * when {@code annotationProcessorPathsUseDepMgmt} is set or the path names no version.
     *
     * @param project the project
     * @param evaluator what evaluates expressions
     * @return the artifacts, and whether the dependency management applies
     * @throws ExpressionEvaluationException if an expression cannot be evaluated
     */
    public static ProcessorPaths processorPaths(MavenProject project, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        Xpp3Dom configuration = compilerConfiguration(project);
        List<Artifact> artifacts = new ArrayList<>();
        boolean managed = false;
        if (configuration != null) {
            managed = Boolean.parseBoolean(childValue(configuration, "annotationProcessorPathsUseDepMgmt", evaluator));
            Xpp3Dom paths = configuration.getChild("annotationProcessorPaths");
            if (paths != null) {
                for (Xpp3Dom path : paths.getChildren()) {
                    String groupId = childValue(path, "groupId", evaluator);
                    String artifactId = childValue(path, "artifactId", evaluator);
                    String version = childValue(path, "version", evaluator);
                    String classifier = childValue(path, "classifier", evaluator);
                    if (groupId == null || artifactId == null) {
                        continue;
                    }
                    if (version == null || version.isEmpty()) {
                        managed = true;
                    }
                    artifacts.add(new DefaultArtifact(groupId, artifactId, classifier == null ? "" : classifier, "jar", version == null ? "" : version));
                }
            }
        }
        return new ProcessorPaths(artifacts, managed);
    }

    /**
     * The javac options as the compiler plugin is configured: {@code -parameters}, the release or the
     * source and target, the encoding, and the compiler arguments.
     */
    static List<String> javacOptions(MavenProject project, ExpressionEvaluator evaluator) {
        List<String> options = new ArrayList<>();
        Xpp3Dom configuration = compilerConfiguration(project);
        try {
            if (Boolean.parseBoolean(setting(configuration, "parameters", "maven.compiler.parameters", project, evaluator))) {
                options.add("-parameters");
            }
            String release = setting(configuration, "release", "maven.compiler.release", project, evaluator);
            if (release != null && !release.isEmpty()) {
                options.add("--release");
                options.add(release);
            } else {
                String source = setting(configuration, "source", "maven.compiler.source", project, evaluator);
                String target = setting(configuration, "target", "maven.compiler.target", project, evaluator);
                if (source != null && !source.isEmpty()) {
                    options.add("-source");
                    options.add(source);
                }
                if (target != null && !target.isEmpty()) {
                    options.add("-target");
                    options.add(target);
                }
            }
            String encoding = setting(configuration, "encoding", "project.build.sourceEncoding", project, evaluator);
            if (encoding != null && !encoding.isEmpty()) {
                options.add("-encoding");
                options.add(encoding);
            }
            if (configuration != null) {
                Xpp3Dom compilerArgs = configuration.getChild("compilerArgs");
                if (compilerArgs != null) {
                    for (Xpp3Dom arg : compilerArgs.getChildren()) {
                        String value = evaluate(arg.getValue(), evaluator);
                        if (value != null && !value.isBlank()) {
                            options.add(value.trim());
                        }
                    }
                }
                String compilerArgument = childValue(configuration, "compilerArgument", evaluator);
                if (compilerArgument != null && !compilerArgument.isBlank()) {
                    options.add(compilerArgument.trim());
                }
            }
        } catch (ExpressionEvaluationException e) {
            throw new IllegalStateException("Cannot read the compiler plugin's configuration: " + e.getMessage(), e);
        }
        return options;
    }

    /**
     * The source and resource roots of a project as configured: the compile source roots, a build-helper's
     * included, the resource directories, and {@code src/main/groovy}.
     *
     * @param project the project
     * @return the roots, which may not exist
     */
    public static List<String> sourceRoots(MavenProject project) {
        Set<String> roots = new LinkedHashSet<>(userSourceRoots(project));
        for (Resource resource : project.getResources()) {
            roots.add(resource.getDirectory());
        }
        roots.add(groovyRoot(project));
        return new ArrayList<>(roots);
    }

    /**
     * The compile source roots a build declares, without those under the build directory: the compiler
     * plugin registers the generated-sources directory as a root once it ran, and what the processors
     * generated is an output of the embedded compilation, not an input to it.
     */
    private static List<String> userSourceRoots(MavenProject project) {
        Path build = Path.of(project.getBuild().getDirectory()).toAbsolutePath().normalize();
        List<String> roots = new ArrayList<>();
        for (String root : project.getCompileSourceRoots()) {
            if (!Path.of(root).toAbsolutePath().normalize().startsWith(build)) {
                roots.add(root);
            }
        }
        return roots;
    }

    private static String groovyRoot(MavenProject project) {
        return new File(project.getBasedir(), "src/main/groovy").getAbsolutePath();
    }

    /**
     * The resource roots read live: a filtered one is not, since the build expands it and its copy is in the output.
     */
    private static List<String> liveResourceRoots(MavenProject project) {
        List<String> roots = new ArrayList<>();
        for (Resource resource : project.getResources()) {
            if (!resource.isFiltering() && new File(resource.getDirectory()).isDirectory()) {
                roots.add(new File(resource.getDirectory()).getAbsolutePath());
            }
        }
        return roots;
    }

    /**
     * The compiler plugin's configuration as the main compilation sees it: the plugin-level configuration
     * with the {@code default-compile} execution's merged over it, where a build commonly puts the
     * processor paths, the release or the compiler arguments.
     */
    private static Xpp3Dom compilerConfiguration(MavenProject project) {
        Plugin plugin = project.getPlugin(COMPILER_PLUGIN);
        if (plugin == null) {
            return null;
        }
        Xpp3Dom configuration = plugin.getConfiguration() instanceof Xpp3Dom dom ? dom : null;
        for (PluginExecution execution : plugin.getExecutions()) {
            if (DEFAULT_COMPILE_EXECUTION.equals(execution.getId()) && execution.getConfiguration() instanceof Xpp3Dom executionDom) {
                configuration = configuration == null ? executionDom : Xpp3Dom.mergeXpp3Dom(new Xpp3Dom(executionDom), configuration);
            }
        }
        return configuration;
    }

    private static String setting(Xpp3Dom configuration, String child, String property, MavenProject project, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        String value = configuration == null ? null : childValue(configuration, child, evaluator);
        if (value == null) {
            value = project.getProperties().getProperty(property);
        }
        return value;
    }

    private static String childValue(Xpp3Dom parent, String name, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        Xpp3Dom child = parent.getChild(name);
        return child == null ? null : evaluate(child.getValue(), evaluator);
    }

    private static String evaluate(String value, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        if (value == null) {
            return null;
        }
        Object evaluated = evaluator.evaluate(value);
        return evaluated == null ? null : evaluated.toString();
    }

    private static void putResourceKind(Map<String, String> entries, String kind, List<String> resources, String... names) {
        Set<String> found = new LinkedHashSet<>();
        for (String resourceDir : resources) {
            for (String name : names) {
                File candidate = new File(resourceDir, name);
                if (candidate.isDirectory()) {
                    found.add(candidate.getAbsolutePath());
                }
            }
        }
        if (!found.isEmpty()) {
            entries.put(PREFIX + "resources." + kind, String.join(File.pathSeparator, found));
        }
    }

    private static List<String> existing(List<String> directories) {
        List<String> existing = new ArrayList<>();
        for (String directory : directories) {
            if (new File(directory).isDirectory()) {
                existing.add(new File(directory).getAbsolutePath());
            }
        }
        return existing;
    }

    private static List<String> paths(List<File> files) {
        List<String> paths = new ArrayList<>(files.size());
        for (File file : files) {
            paths.add(file.getAbsolutePath());
        }
        return paths;
    }

    private String argumentFile(String name, List<String> lines) {
        argumentFiles.put(name, lines);
        return "@" + name;
    }

    /**
     * The settings of the goal that go into the manifest.
     *
     * @param mainClass the application's main class
     * @param strategy the reload strategy
     * @param compile the compile mode
     * @param incremental whether compilation is incremental
     * @param retain the types retained across a restart
     * @param liveReloadPort the LiveReload port
     * @param liveReloadInjectScript whether the LiveReload script is injected
     */
    public record Settings(String mainClass, String strategy, String compile, boolean incremental, List<String> retain, int liveReloadPort, boolean liveReloadInjectScript) {
    }

    /**
     * The annotation processor path to resolve.
     *
     * @param artifacts the artifacts, some without a version
     * @param managed whether the dependency management supplies versions
     */
    public record ProcessorPaths(List<Artifact> artifacts, boolean managed) {
    }
}
