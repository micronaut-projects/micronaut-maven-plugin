package io.micronaut.maven.dev;

import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Resource;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.Exclusion;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class DevManifestTest {

    private static final String PREFIX = "micronaut.dev.";
    private static final DevManifest.Settings SETTINGS = new DevManifest.Settings("example.Application", "auto", "embedded", true, List.of("example.Pool", "example.Client"), 35729, true);

    @TempDir
    Path directory;

    @Test
    void theManifestHoldsTheClasspathsRootsAndSettings() throws Exception {
        MavenProject app = project(directory.resolve("app"), "app");
        Path appSources = Files.createDirectories(directory.resolve("app/src/main/java"));
        Path appResources = Files.createDirectories(directory.resolve("app/src/main/resources"));
        Files.createDirectories(appResources.resolve("views"));
        Files.createDirectories(appResources.resolve("static"));
        Files.createDirectories(appResources.resolve("public"));
        Files.createDirectories(appResources.resolve("i18n"));
        Path filtered = Files.createDirectories(directory.resolve("app/src/main/filtered"));
        Path groovy = Files.createDirectories(directory.resolve("app/src/main/groovy"));
        app.addCompileSourceRoot(appSources.toString());
        // the compiler plugin registers the generated sources once it ran: they are an output, not a source
        app.addCompileSourceRoot(directory.resolve("app/target/generated-sources/annotations").toString());
        app.addResource(resource(appResources, false));
        app.addResource(resource(filtered, true));
        app.getBuild().addPlugin(compilerPlugin(configuration(Map.of("release", "25", "parameters", "true", "encoding", "UTF-8"))));

        MavenProject lib = project(directory.resolve("lib"), "lib");
        Path libSources = Files.createDirectories(directory.resolve("lib/src/main/java"));
        lib.addCompileSourceRoot(libSources.toString());
        lib.addCompileSourceRoot(directory.resolve("lib/src/main/missing").toString());

        File runtimeJar = directory.resolve("runtime.jar").toFile();
        File compileJar = directory.resolve("compile.jar").toFile();
        File processorJar = directory.resolve("processor.jar").toFile();
        Path manifestDirectory = directory.resolve("app/target/micronaut-dev");
        DevManifest manifest = DevManifest.of(app, List.of(app, lib), SETTINGS, List.of(runtimeJar), List.of(compileJar), List.of(processorJar), evaluator(Map.of()), manifestDirectory);

        Map<String, String> entries = manifest.entries();
        assertEquals("example.Application", entries.get(PREFIX + "main-class"));
        assertEquals(app.getBasedir().getAbsolutePath(), entries.get(PREFIX + "project-dir"));
        assertEquals("auto", entries.get(PREFIX + "strategy"));
        assertEquals("@runtime.argfile", entries.get(PREFIX + "runtime-classpath"));
        assertEquals("@compile.argfile", entries.get(PREFIX + "compile-classpath"));
        assertEquals("@processors.argfile", entries.get(PREFIX + "processor-path"));
        assertEquals(String.join(File.pathSeparator, app.getBuild().getOutputDirectory(), lib.getBuild().getOutputDirectory()), entries.get(PREFIX + "reloadable"));
        assertEquals(String.join(File.pathSeparator, appSources.toString(), libSources.toString()), entries.get(PREFIX + "sources.java"));
        assertEquals(groovy.toString(), entries.get(PREFIX + "sources.groovy"));
        assertEquals(appResources.toString(), entries.get(PREFIX + "resources.config"), "a filtered resource root is not read live");
        assertEquals(appResources.resolve("views").toString(), entries.get(PREFIX + "resources.views"));
        assertEquals(String.join(File.pathSeparator, appResources.resolve("static").toString(), appResources.resolve("public").toString()), entries.get(PREFIX + "resources.static"));
        assertEquals(appResources.resolve("i18n").toString(), entries.get(PREFIX + "resources.i18n"));
        assertEquals("embedded", entries.get(PREFIX + "compile.mode"));
        assertEquals("true", entries.get(PREFIX + "compile.incremental"));
        assertEquals(app.getBuild().getOutputDirectory(), entries.get(PREFIX + "compile.java.output"));
        assertEquals(directory.resolve("app/target/generated-sources/annotations").toString(), entries.get(PREFIX + "compile.java.generated-sources"));
        assertEquals("@java-options.argfile", entries.get(PREFIX + "compile.java.options"));
        assertEquals(app.getBuild().getOutputDirectory(), entries.get(PREFIX + "compile.groovy.output"));
        assertEquals("maven", entries.get(PREFIX + "build-tool"));
        assertEquals(directory.resolve("app/target/micronaut-dev/reload").toString(), entries.get(PREFIX + "build-tool.trigger"));
        assertEquals(manifestDirectory.resolve(DevManifest.GENERATIONS_DIRECTORY_NAME).toString(), entries.get(PREFIX + "generations"));
        assertEquals("example.Pool,example.Client", entries.get(PREFIX + "retain"));
        assertEquals("35729", entries.get(PREFIX + "livereload.port"));
        assertEquals("true", entries.get(PREFIX + "livereload.inject-script"));

        Path file = manifest.writeTo(manifestDirectory);
        assertEquals(manifestDirectory.resolve(DevManifest.FILE_NAME), file);
        Properties written = new Properties();
        try (Reader reader = Files.newBufferedReader(file)) {
            written.load(reader);
        }
        assertEquals(entries.size(), written.size());
        assertEquals("example.Application", written.getProperty(PREFIX + "main-class"));
        assertEquals(List.of(runtimeJar.getAbsolutePath()), Files.readAllLines(manifestDirectory.resolve("runtime.argfile")));
        assertEquals(List.of(compileJar.getAbsolutePath()), Files.readAllLines(manifestDirectory.resolve("compile.argfile")));
        assertEquals(List.of(processorJar.getAbsolutePath()), Files.readAllLines(manifestDirectory.resolve("processors.argfile")));
        assertEquals(List.of("-parameters", "--release", "25", "-encoding", "UTF-8"), Files.readAllLines(manifestDirectory.resolve("java-options.argfile")));
    }

    @Test
    void aProjectWithoutSourcesHasNoCompilationEntries() throws Exception {
        MavenProject app = project(directory, "app");
        DevManifest.Settings settings = new DevManifest.Settings("example.Application", "restart", "build-tool", false, List.of(), 1234, false);

        Map<String, String> entries = DevManifest.of(app, List.of(app), settings, List.of(), List.of(), List.of(), evaluator(Map.of()), directory.resolve("target/micronaut-dev")).entries();

        assertFalse(entries.containsKey(PREFIX + "sources.java"));
        assertFalse(entries.containsKey(PREFIX + "sources.groovy"));
        assertFalse(entries.containsKey(PREFIX + "resources.config"));
        assertFalse(entries.containsKey(PREFIX + "compile.java.output"));
        assertFalse(entries.containsKey(PREFIX + "compile.groovy.output"));
        assertFalse(entries.containsKey(PREFIX + "retain"));
        assertEquals("build-tool", entries.get(PREFIX + "compile.mode"));
        assertEquals("false", entries.get(PREFIX + "compile.incremental"));
        assertEquals("1234", entries.get(PREFIX + "livereload.port"));
        assertEquals("false", entries.get(PREFIX + "livereload.inject-script"));
    }

    @Test
    void theJavacOptionsFallBackToTheProjectProperties() throws Exception {
        MavenProject project = project(directory, "app");
        project.getProperties().setProperty("maven.compiler.source", "21");
        project.getProperties().setProperty("maven.compiler.target", "21");
        project.getProperties().setProperty("maven.compiler.parameters", "true");
        project.getProperties().setProperty("project.build.sourceEncoding", "ISO-8859-1");

        assertEquals(List.of("-parameters", "-source", "21", "-target", "21", "-encoding", "ISO-8859-1"), DevManifest.javacOptions(project, evaluator(Map.of()), false));
    }

    @Test
    void theDefaultCompileExecutionIsMergedOverThePluginConfiguration() throws Exception {
        MavenProject project = project(directory, "app");
        Xpp3Dom pluginConfiguration = configuration(Map.of("release", "17", "compilerArgument", " -Xlint:all "));
        Xpp3Dom compilerArgs = new Xpp3Dom("compilerArgs");
        compilerArgs.addChild(value("arg", "${processing.group}"));
        compilerArgs.addChild(value("arg", " "));
        compilerArgs.addChild(new Xpp3Dom("arg"));
        pluginConfiguration.addChild(compilerArgs);
        Plugin plugin = compilerPlugin(pluginConfiguration);
        plugin.addExecution(execution("default-testCompile", configuration(Map.of("release", "8"))));
        plugin.addExecution(execution("default-compile", configuration(Map.of("release", "${java.release}"))));
        project.getBuild().addPlugin(plugin);

        List<String> options = DevManifest.javacOptions(project, evaluator(Map.of("${processing.group}", "-Amicronaut.processing.group=example", "${java.release}", "25")), false);

        assertEquals(List.of("--release", "25", "-Amicronaut.processing.group=example", "-Xlint:all"), options);
    }

    @Test
    void anExpressionThatCannotBeEvaluatedFailsTheJavacOptions() throws Exception {
        MavenProject project = project(directory, "app");
        project.getBuild().addPlugin(compilerPlugin(configuration(Map.of("release", "${broken}"))));
        ExpressionEvaluator evaluator = mock(ExpressionEvaluator.class);
        when(evaluator.evaluate(anyString())).thenThrow(new ExpressionEvaluationException("broken"));

        IllegalStateException failure = assertThrows(IllegalStateException.class, () -> DevManifest.javacOptions(project, evaluator, false));
        assertTrue(failure.getMessage().contains("broken"));
    }

    @Test
    void theProcessorPathsAreReadWithTheirExclusions() throws Exception {
        MavenProject project = project(directory, "app");
        Xpp3Dom paths = new Xpp3Dom("annotationProcessorPaths");
        Xpp3Dom inject = path("io.micronaut", "micronaut-inject-java", "${micronaut.version}", null);
        Xpp3Dom exclusions = new Xpp3Dom("exclusions");
        exclusions.addChild(configuration("exclusion", Map.of("groupId", "org.slf4j", "artifactId", "slf4j-api")));
        exclusions.addChild(configuration("exclusion", Map.of("groupId", "io.netty")));
        inject.addChild(exclusions);
        paths.addChild(inject);
        paths.addChild(path("io.micronaut.validation", "micronaut-validation-processor", "1.0.0", "tests"));
        // a path naming no artifact is left out
        paths.addChild(configuration("path", Map.of("groupId", "io.micronaut")));
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(paths);
        project.getBuild().addPlugin(compilerPlugin(configuration));

        DevManifest.ProcessorPaths processorPaths = DevManifest.processorPaths(project, evaluator(Map.of("${micronaut.version}", "5.0.0")));

        assertFalse(processorPaths.managed(), "every path names a version");
        assertEquals(2, processorPaths.dependencies().size());
        Dependency first = processorPaths.dependencies().get(0);
        assertEquals("io.micronaut:micronaut-inject-java:jar:5.0.0", first.getArtifact().toString());
        assertEquals("runtime", first.getScope());
        assertEquals(List.of(new Exclusion("org.slf4j", "slf4j-api", "*", "*"), new Exclusion("io.netty", "*", "*", "*")), List.copyOf(first.getExclusions()));
        Dependency second = processorPaths.dependencies().get(1);
        assertEquals("io.micronaut.validation:micronaut-validation-processor:jar:tests:1.0.0", second.getArtifact().toString());
        assertTrue(second.getExclusions().isEmpty());
    }

    @Test
    void aProcessorPathWithoutAVersionIsManaged() throws Exception {
        MavenProject project = project(directory, "app");
        Xpp3Dom paths = new Xpp3Dom("annotationProcessorPaths");
        paths.addChild(path("io.micronaut", "micronaut-inject-java", null, null));
        Xpp3Dom configuration = new Xpp3Dom("configuration");
        configuration.addChild(paths);
        project.getBuild().addPlugin(compilerPlugin(configuration));

        DevManifest.ProcessorPaths processorPaths = DevManifest.processorPaths(project, evaluator(Map.of()));

        assertTrue(processorPaths.managed());
        assertEquals("", processorPaths.dependencies().get(0).getArtifact().getVersion());
    }

    @Test
    void theDependencyManagementIsAppliedWhenTheCompilerPluginAsksForIt() throws Exception {
        MavenProject project = project(directory, "app");
        project.getBuild().addPlugin(compilerPlugin(configuration(Map.of("annotationProcessorPathsUseDepMgmt", "true"))));

        DevManifest.ProcessorPaths processorPaths = DevManifest.processorPaths(project, evaluator(Map.of()));

        assertTrue(processorPaths.managed());
        assertTrue(processorPaths.dependencies().isEmpty());
    }

    @Test
    void aProjectWithoutTheCompilerPluginHasNoProcessorPath() throws Exception {
        DevManifest.ProcessorPaths processorPaths = DevManifest.processorPaths(project(directory, "app"), evaluator(Map.of()));

        assertFalse(processorPaths.managed());
        assertTrue(processorPaths.dependencies().isEmpty());
    }

    @Test
    void theSourceRootsAreTheConfiguredSourceAndResourceRoots() throws Exception {
        MavenProject project = project(directory, "app");
        String sources = directory.resolve("src/main/java").toString();
        String extra = directory.resolve("src/extra/java").toString();
        String resources = directory.resolve("src/main/resources").toString();
        project.addCompileSourceRoot(sources);
        project.addCompileSourceRoot(extra);
        project.addCompileSourceRoot(directory.resolve("target/generated-sources/annotations").toString());
        project.addResource(resource(Path.of(resources), false));

        assertEquals(List.of(sources, extra, resources, directory.resolve("src/main/groovy").toString()), DevManifest.sourceRoots(project));
    }

    private static MavenProject project(Path basedir, String artifactId) throws IOException {
        Files.createDirectories(basedir);
        MavenProject project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId(artifactId);
        project.setFile(basedir.resolve("pom.xml").toFile());
        project.getBuild().setDirectory(basedir.resolve("target").toString());
        project.getBuild().setOutputDirectory(basedir.resolve("target/classes").toString());
        return project;
    }

    private static Resource resource(Path directory, boolean filtering) {
        Resource resource = new Resource();
        resource.setDirectory(directory.toString());
        resource.setFiltering(filtering);
        return resource;
    }

    private static Plugin compilerPlugin(Xpp3Dom configuration) {
        Plugin plugin = new Plugin();
        plugin.setGroupId("org.apache.maven.plugins");
        plugin.setArtifactId("maven-compiler-plugin");
        plugin.setConfiguration(configuration);
        return plugin;
    }

    private static PluginExecution execution(String id, Xpp3Dom configuration) {
        PluginExecution execution = new PluginExecution();
        execution.setId(id);
        execution.setConfiguration(configuration);
        return execution;
    }

    private static Xpp3Dom path(String groupId, String artifactId, String version, String classifier) {
        Xpp3Dom path = new Xpp3Dom("path");
        path.addChild(value("groupId", groupId));
        path.addChild(value("artifactId", artifactId));
        if (version != null) {
            path.addChild(value("version", version));
        }
        if (classifier != null) {
            path.addChild(value("classifier", classifier));
        }
        return path;
    }

    private static Xpp3Dom configuration(Map<String, String> values) {
        return configuration("configuration", values);
    }

    private static Xpp3Dom configuration(String name, Map<String, String> values) {
        Xpp3Dom configuration = new Xpp3Dom(name);
        values.entrySet().stream().sorted(Map.Entry.comparingByKey()).forEach(entry -> configuration.addChild(value(entry.getKey(), entry.getValue())));
        return configuration;
    }

    private static Xpp3Dom value(String name, String value) {
        Xpp3Dom dom = new Xpp3Dom(name);
        dom.setValue(value);
        return dom;
    }

    /**
     * An evaluator replacing the whole expressions it knows, and returning anything else as is.
     */
    private static ExpressionEvaluator evaluator(Map<String, String> expressions) throws ExpressionEvaluationException {
        ExpressionEvaluator evaluator = mock(ExpressionEvaluator.class);
        when(evaluator.evaluate(anyString())).thenAnswer(invocation -> {
            String expression = invocation.getArgument(0);
            return expressions.getOrDefault(expression, expression);
        });
        return evaluator;
    }
}
