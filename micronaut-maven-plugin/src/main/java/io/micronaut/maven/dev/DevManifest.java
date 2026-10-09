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
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.Exclusion;
import org.eclipse.aether.util.artifact.JavaScopes;

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

    /**
     * The name of the directory beside the manifest holding the launcher's generations.
     */
    public static final String GENERATIONS_DIRECTORY_NAME = "generations";

    private static final String PREFIX = "micronaut.dev.";
    private static final String COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    private static final String DEFAULT_COMPILE_EXECUTION = "default-compile";
    private static final String DEFAULT_TEST_COMPILE_EXECUTION = "default-testCompile";
    private static final String GENERATED_SOURCES = "generated-sources/annotations";
    private static final String GENERATED_TEST_SOURCES = "generated-test-sources/test-annotations";

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
     * @param directory the directory the manifest is written to, which holds the trigger file
     * @return the manifest
     */
    public static DevManifest of(MavenProject project,
                                 List<MavenProject> reactor,
                                 Settings settings,
                                 List<File> runtimeClasspath,
                                 List<File> compileClasspath,
                                 List<File> processorPath,
                                 ExpressionEvaluator evaluator,
                                 Path directory) {
        DevManifest manifest = new DevManifest();
        Path build = Path.of(project.getBuild().getDirectory());
        // a test run has no application main: the launcher only requires one in run mode
        if (settings.mainClass() != null) {
            manifest.entries.put(PREFIX + "main-class", settings.mainClass());
        }
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
            List<String> options = javacOptions(project, evaluator, false);
            if (!options.isEmpty()) {
                manifest.entries.put(PREFIX + "compile.java.options", manifest.argumentFile("java-options.argfile", options));
            }
        }
        if (!groovySources.isEmpty()) {
            manifest.entries.put(PREFIX + "compile.groovy.output", output);
        }
        manifest.entries.put(PREFIX + "build-tool", "maven");
        manifest.entries.put(PREFIX + "build-tool.trigger", directory.resolve(TRIGGER_FILE_NAME).toString());
        // the snapshots of the reloadable roots go beside the manifest, under target, never into a build directory
        manifest.entries.put(PREFIX + "generations", directory.resolve(GENERATIONS_DIRECTORY_NAME).toString());
        if (!settings.retain().isEmpty()) {
            manifest.entries.put(PREFIX + "retain", String.join(",", settings.retain()));
        }
        manifest.entries.put(PREFIX + "livereload.port", String.valueOf(settings.liveReloadPort()));
        manifest.entries.put(PREFIX + "livereload.inject-script", String.valueOf(settings.liveReloadInjectScript()));
        return manifest;
    }

    /**
     * Switches the manifest to test mode: the runtime runs the project's tests instead of its application. The
     * test sources and resources of the project, their output, the test compile classpath and processor path,
     * and the settings of the {@code mn:test} goal are added to the entries of run mode, which stay.
     *
     * @param project the project whose tests run
     * @param settings the goal's test settings
     * @param testCompileClasspath the test compile classpath: the reloadable outputs first, then the modules
     * @param testProcessorPath the test annotation processor path
     * @param reactorTestProjects the reactor projects whose test-jar the tests depend on: their test sources compile
     *                            into the project's test output, which the generation reads first, as a dependency's
     *                            sources compile into the application's output
     * @param evaluator what evaluates the expressions of the compiler plugin's configuration
     * @return this manifest
     */
    public DevManifest withTests(MavenProject project,
                                 TestSettings settings,
                                 List<File> testCompileClasspath,
                                 List<File> testProcessorPath,
                                 List<MavenProject> reactorTestProjects,
                                 ExpressionEvaluator evaluator) {
        Path build = Path.of(project.getBuild().getDirectory());
        entries.put(PREFIX + "mode", "test");
        List<String> javaSources = new ArrayList<>(existing(userTestSourceRoots(project)));
        List<String> groovySources = new ArrayList<>(existing(List.of(groovyTestRoot(project))));
        List<String> resources = new ArrayList<>(liveTestResourceRoots(project));
        for (MavenProject reactorProject : reactorTestProjects) {
            javaSources.addAll(existing(userTestSourceRoots(reactorProject)));
            groovySources.addAll(existing(List.of(groovyTestRoot(reactorProject))));
            resources.addAll(liveTestResourceRoots(reactorProject));
        }
        if (!javaSources.isEmpty()) {
            entries.put(PREFIX + "test.sources.java", String.join(File.pathSeparator, javaSources));
        }
        if (!groovySources.isEmpty()) {
            entries.put(PREFIX + "test.sources.groovy", String.join(File.pathSeparator, groovySources));
        }
        if (!resources.isEmpty()) {
            entries.put(PREFIX + "test.resources.config", String.join(File.pathSeparator, resources));
        }
        // the tests compile into the test output whatever their language, as the build does
        String output = project.getBuild().getTestOutputDirectory();
        // the test output is reloadable, ahead of the application's as on the build's test classpath, even when no
        // test source root is watched: tests generated under the build directory were compiled there by the build
        List<String> reloadable = new ArrayList<>();
        reloadable.add(output);
        reloadable.add(entries.get(PREFIX + "reloadable"));
        // a sibling's test-jar loads from its test output, after the main outputs its classes use
        for (MavenProject reactorProject : reactorTestProjects) {
            reloadable.add(reactorProject.getBuild().getTestOutputDirectory());
        }
        entries.put(PREFIX + "reloadable", String.join(File.pathSeparator, reloadable));
        if (!javaSources.isEmpty()) {
            entries.put(PREFIX + "test.compile.java.output", output);
            entries.put(PREFIX + "test.compile.java.generated-sources", build.resolve(GENERATED_TEST_SOURCES).toString());
            List<String> options = javacOptions(project, evaluator, true);
            if (!options.isEmpty()) {
                entries.put(PREFIX + "test.compile.java.options", argumentFile("test-java-options.argfile", options));
            }
        }
        if (!groovySources.isEmpty()) {
            entries.put(PREFIX + "test.compile.groovy.output", output);
        }
        entries.put(PREFIX + "test.compile-classpath", argumentFile("test-compile.argfile", paths(testCompileClasspath)));
        entries.put(PREFIX + "test.processor-path", argumentFile("test-processors.argfile", paths(testProcessorPath)));
        entries.put(PREFIX + "test.runner", settings.runner());
        entries.put(PREFIX + "test.selection", settings.selection());
        entries.put(PREFIX + "test.initial-run", String.valueOf(settings.initialRun()));
        entries.put(PREFIX + "test.once", String.valueOf(settings.once()));
        entries.put(PREFIX + "test.reports", settings.reports().toString());
        entries.put(PREFIX + "test.html-report", settings.htmlReport().toString());
        entries.put(PREFIX + "test.html-report-path", settings.reportPath());
        if (!settings.filter().isEmpty()) {
            entries.put(PREFIX + "test.filter", String.join(",", settings.filter()));
        }
        settings.parameters().forEach((key, value) -> entries.put(PREFIX + "test.parameters." + key, value));
        return this;
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
     * {@code annotationProcessorPaths}, as dependencies to resolve with the exclusions the path declares,
     * versioned by the dependency management when {@code annotationProcessorPathsUseDepMgmt} is set or the
     * path names no version.
     *
     * @param project the project
     * @param evaluator what evaluates expressions
     * @return the artifacts, and whether the dependency management applies
     * @throws ExpressionEvaluationException if an expression cannot be evaluated
     */
    public static ProcessorPaths processorPaths(MavenProject project, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        Xpp3Dom configuration = compilerConfiguration(project, DEFAULT_COMPILE_EXECUTION);
        return processorPaths(configuration, configuration == null ? null : configuration.getChild("annotationProcessorPaths"), evaluator);
    }

    /**
     * The annotation processor path the tests compile with, as the compiler plugin's {@code testCompile} reads it:
     * {@code annotationProcessorPaths}, with the {@code default-testCompile} execution's configuration merged over
     * the plugin's, where a build declares processors for its tests only.
     *
     * @param project the project
     * @param evaluator what evaluates expressions
     * @return the artifacts, and whether the dependency management applies
     * @throws ExpressionEvaluationException if an expression cannot be evaluated
     */
    public static ProcessorPaths testProcessorPaths(MavenProject project, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        Xpp3Dom configuration = compilerConfiguration(project, DEFAULT_TEST_COMPILE_EXECUTION);
        return processorPaths(configuration, configuration == null ? null : configuration.getChild("annotationProcessorPaths"), evaluator);
    }

    private static ProcessorPaths processorPaths(Xpp3Dom configuration, Xpp3Dom paths, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        List<Dependency> dependencies = new ArrayList<>();
        boolean managed = false;
        if (configuration != null) {
            managed = Boolean.parseBoolean(childValue(configuration, "annotationProcessorPathsUseDepMgmt", evaluator));
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
                    Artifact artifact = new DefaultArtifact(groupId, artifactId, classifier == null ? "" : classifier, "jar", version == null ? "" : version);
                    dependencies.add(new Dependency(artifact, JavaScopes.RUNTIME, false, exclusions(path, evaluator)));
                }
            }
        }
        return new ProcessorPaths(dependencies, managed);
    }

    /**
     * The exclusions of a processor path entry, as Maven applies them: every classifier and extension of the
     * excluded module, and a {@code *} matching any group or artifact.
     */
    private static List<Exclusion> exclusions(Xpp3Dom path, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        Xpp3Dom exclusions = path.getChild("exclusions");
        if (exclusions == null) {
            return List.of();
        }
        List<Exclusion> result = new ArrayList<>();
        for (Xpp3Dom exclusion : exclusions.getChildren("exclusion")) {
            String groupId = childValue(exclusion, "groupId", evaluator);
            String artifactId = childValue(exclusion, "artifactId", evaluator);
            result.add(new Exclusion(groupId == null ? "*" : groupId, artifactId == null ? "*" : artifactId, "*", "*"));
        }
        return result;
    }

    /**
     * The javac options as the compiler plugin is configured: {@code -parameters}, the release or the
     * source and target, the encoding, and the compiler arguments. For the tests, the {@code default-testCompile}
     * execution's configuration applies, and {@code testRelease}, {@code testSource}, {@code testTarget} and
     * {@code testCompilerArgument} and {@code testCompilerArguments} win over their main counterparts, as
     * {@code testCompile} reads them.
     */
    static List<String> javacOptions(MavenProject project, ExpressionEvaluator evaluator, boolean test) {
        List<String> options = new ArrayList<>();
        Xpp3Dom configuration = compilerConfiguration(project, test ? DEFAULT_TEST_COMPILE_EXECUTION : DEFAULT_COMPILE_EXECUTION);
        try {
            if (Boolean.parseBoolean(setting(configuration, "parameters", "maven.compiler.parameters", project, evaluator))) {
                options.add("-parameters");
            }
            String release = compilerSetting(configuration, "release", test, project, evaluator);
            if (release != null && !release.isEmpty()) {
                options.add("--release");
                options.add(release);
            } else {
                String source = compilerSetting(configuration, "source", test, project, evaluator);
                String target = compilerSetting(configuration, "target", test, project, evaluator);
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
                String compilerArgument = test ? childValue(configuration, "testCompilerArgument", evaluator) : null;
                if (compilerArgument == null) {
                    compilerArgument = childValue(configuration, "compilerArgument", evaluator);
                }
                if (compilerArgument != null && !compilerArgument.isBlank()) {
                    options.add(compilerArgument.trim());
                }
                // the map form, each key an option and each value its argument, the tests' own when configured
                Xpp3Dom compilerArguments = test ? configuration.getChild("testCompilerArguments") : null;
                if (compilerArguments == null) {
                    compilerArguments = configuration.getChild("compilerArguments");
                }
                if (compilerArguments != null) {
                    for (Xpp3Dom argument : compilerArguments.getChildren()) {
                        String name = argument.getName();
                        options.add(name.startsWith("-") ? name : "-" + name);
                        String value = evaluate(argument.getValue(), evaluator);
                        if (value != null && !value.isBlank()) {
                            options.add(value.trim());
                        }
                    }
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
     * The test source and resource roots of a project as configured: the test compile source roots, the test
     * resource directories, and {@code src/test/groovy}.
     *
     * @param project the project
     * @return the roots, which may not exist
     */
    public static List<String> testSourceRoots(MavenProject project) {
        Set<String> roots = new LinkedHashSet<>(userTestSourceRoots(project));
        for (Resource resource : project.getTestResources()) {
            roots.add(resource.getDirectory());
        }
        roots.add(groovyTestRoot(project));
        return new ArrayList<>(roots);
    }

    /**
     * The test compile source roots a build declares, without the generated ones under the build directory.
     */
    private static List<String> userTestSourceRoots(MavenProject project) {
        Path build = Path.of(project.getBuild().getDirectory()).toAbsolutePath().normalize();
        List<String> roots = new ArrayList<>();
        for (String root : project.getTestCompileSourceRoots()) {
            if (!Path.of(root).toAbsolutePath().normalize().startsWith(build)) {
                roots.add(root);
            }
        }
        return roots;
    }

    private static String groovyTestRoot(MavenProject project) {
        return new File(project.getBasedir(), "src/test/groovy").getAbsolutePath();
    }

    /**
     * The test resource roots read live, a filtered one excepted, as for the main resources.
     */
    private static List<String> liveTestResourceRoots(MavenProject project) {
        List<String> roots = new ArrayList<>();
        for (Resource resource : project.getTestResources()) {
            if (!resource.isFiltering() && new File(resource.getDirectory()).isDirectory()) {
                roots.add(new File(resource.getDirectory()).getAbsolutePath());
            }
        }
        return roots;
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
     * The compiler plugin's configuration as a compilation sees it: the plugin-level configuration with the
     * execution's, {@code default-compile} or {@code default-testCompile}, merged over it, where a build
     * commonly puts the processor paths, the release or the compiler arguments.
     */
    private static Xpp3Dom compilerConfiguration(MavenProject project, String executionId) {
        Plugin plugin = project.getPlugin(COMPILER_PLUGIN);
        if (plugin == null) {
            return null;
        }
        Xpp3Dom configuration = plugin.getConfiguration() instanceof Xpp3Dom dom ? dom : null;
        for (PluginExecution execution : plugin.getExecutions()) {
            if (executionId.equals(execution.getId()) && execution.getConfiguration() instanceof Xpp3Dom executionDom) {
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

    /**
     * A setting of the compiler plugin, {@code release}, {@code source} or {@code target}: for the tests its
     * {@code test} counterpart and property first, as {@code testCompile} reads them.
     */
    private static String compilerSetting(Xpp3Dom configuration, String name, boolean test, MavenProject project, ExpressionEvaluator evaluator) throws ExpressionEvaluationException {
        if (test) {
            String testName = "test" + Character.toUpperCase(name.charAt(0)) + name.substring(1);
            String value = setting(configuration, testName, "maven.compiler." + testName, project, evaluator);
            if (value != null && !value.isEmpty()) {
                return value;
            }
        }
        return setting(configuration, name, "maven.compiler." + name, project, evaluator);
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
     * The settings of the {@code mn:test} goal that go into the manifest.
     *
     * @param runner the test runner, {@code junit-platform}
     * @param selection the tests a change runs, {@code affected} or {@code all}
     * @param initialRun whether every test runs at start
     * @param once whether the launcher exits after one run, with its status
     * @param reports the directory of the JUnit XML reports
     * @param htmlReport the directory of the HTML report
     * @param reportPath the path the LiveReload server serves the HTML report at
     * @param filter the test patterns: globs, classes or {@code Class.method}
     * @param parameters the JUnit Platform configuration parameters
     */
    public record TestSettings(String runner, String selection, boolean initialRun, boolean once, Path reports, Path htmlReport,
                               String reportPath, List<String> filter, Map<String, String> parameters) {
    }

    /**
     * The annotation processor path to resolve.
     *
     * @param dependencies the dependencies, some without a version, with their exclusions
     * @param managed whether the dependency management supplies versions
     */
    public record ProcessorPaths(List<Dependency> dependencies, boolean managed) {
    }
}
