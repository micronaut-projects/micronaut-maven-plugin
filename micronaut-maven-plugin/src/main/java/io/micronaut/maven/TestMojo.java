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

import io.micronaut.maven.dev.DevManifest;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.PluginParameterExpressionEvaluator;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.util.artifact.JavaScopes;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Runs the project's tests continuously through {@code io.micronaut.dev.MicronautDevMain} in test mode: the
 * launcher watches the sources, compiles a change inside its JVM, and runs the tests the change can affect on a
 * new generation, so the libraries, the compilers and the JIT stay warm from one run to the next. Each run writes
 * JUnit XML reports, in the Surefire shape, and a live HTML report.
 *
 * <p>The goal writes a test-mode manifest into {@code target/micronaut-dev/test}, with everything {@code mn:dev}
 * writes plus the test sources, their output, the test classpaths and the settings below, then launches the JVM
 * with the test runtime classpath, the launcher, {@code micronaut-dev-test-report} and
 * {@code junit-platform-launcher}. On a terminal, space runs the last tests again, {@code a} every test,
 * {@code f} the failures, {@code w} turns watching on or off and {@code q} quits. The build fails when the last
 * run did not pass.</p>
 *
 * @since 5.1.0
 */
@SuppressWarnings("unused")
@Mojo(name = TestMojo.MOJO_NAME, requiresDependencyResolution = ResolutionScope.TEST, defaultPhase = LifecyclePhase.TEST, aggregator = true)
@Execute(phase = LifecyclePhase.PROCESS_TEST_CLASSES)
public class TestMojo extends DevMojo {

    /**
     * The name of the goal.
     */
    public static final String MOJO_NAME = "test";

    /**
     * The name of the directory under {@code target/micronaut-dev} holding the test-mode manifest, apart from
     * {@code mn:dev}'s, whose runtime classpath differs.
     */
    public static final String DIRECTORY_NAME = "test";

    private static final String TEST_REPORT_ARTIFACT = "micronaut-dev-test-report";
    private static final String JUNIT_PLATFORM_GROUP = "org.junit.platform";
    private static final String JUNIT_PLATFORM_LAUNCHER = "junit-platform-launcher";
    private static final String JUNIT_PLATFORM_ENGINE = "junit-platform-engine";

    /**
     * The {@code junit-platform-launcher} version used when the test classpath has no JUnit Platform and the
     * dependency management names none: the version micronaut-dev is built against.
     */
    private static final String DEFAULT_JUNIT_PLATFORM_VERSION = "1.12.2";

    /**
     * Runs the tests once and exits with their status, for CI.
     */
    @Parameter(property = "mn.test.once", defaultValue = "false")
    private boolean once;

    /**
     * The tests a change runs: {@code affected}, those that reference a changed class and those that failed, or {@code all}.
     */
    @Parameter(property = "mn.test.selection", defaultValue = "affected")
    private String selection;

    /**
     * Whether every test runs at start.
     */
    @Parameter(property = "mn.test.initialRun", defaultValue = "true")
    private boolean initialRun;

    /**
     * The test runner, a {@code TestRunner} service of the launcher's classpath.
     */
    @Parameter(property = "mn.test.runner", defaultValue = "junit-platform")
    private String runner;

    /**
     * The tests to run, comma separated: globs, a class or {@code Class.method}. A class matches by its binary or
     * its simple name, and {@code *} matches any characters, dots included.
     */
    @Parameter(property = "mn.test.filter")
    private String filter;

    /**
     * The tests to run, as Surefire reads them: {@code Class}, {@code Class#method}, {@code Class#m1+m2} or a
     * pattern, comma separated. Mapped to the filter, alongside {@code mn.test.filter}; exclusions and
     * {@code %regex[]} patterns are not supported.
     */
    @Parameter(property = "test")
    private String test;

    /**
     * The directory of the JUnit XML reports, one {@code TEST-<class>.xml} per class; by default the
     * {@code target/surefire-reports} of the project whose tests run.
     */
    @Parameter(property = "mn.test.reportsDirectory")
    private String reportsDirectory;

    /**
     * The directory of the HTML report, {@code index.html}, which the LiveReload server also serves at
     * {@code /tests/}; by default {@code target/micronaut-dev/test-report} of the project whose tests run.
     */
    @Parameter(property = "mn.test.htmlReportDirectory")
    private String htmlReportDirectory;

    /**
     * The path the LiveReload server serves the HTML report at; the launcher adds the leading and trailing slashes,
     * and rejects {@code /}, {@code ..} and the server's own paths.
     */
    @Parameter(property = "mn.test.reportPath", defaultValue = "/tests/")
    private String reportPath;

    /**
     * Whether {@code micronaut-dev-test-report}, which writes the HTML report, is added to the launcher's classpath.
     */
    @Parameter(property = "mn.test.htmlReport", defaultValue = "true")
    private boolean htmlReport;

    /**
     * JUnit Platform configuration parameters, such as {@code junit.jupiter.execution.parallel.enabled}.
     */
    @Parameter
    private Map<String, String> configurationParameters;

    /**
     * Constructor.
     *
     * @param mavenSession the session
     * @param toolchainManager the toolchain manager
     * @param compilerService the compiler service
     * @param executorService the executor service
     * @param repositorySystem the repository system
     * @param configurator Maven's configurator of mojos, which applies the selected application's configuration
     */
    @Inject
    public TestMojo(MavenSession mavenSession,
                    ToolchainManager toolchainManager,
                    CompilerService compilerService,
                    ExecutorService executorService,
                    RepositorySystem repositorySystem,
                    @Named("basic") ComponentConfigurator configurator) {
        super(mavenSession, toolchainManager, compilerService, executorService, repositorySystem, configurator);
    }

    @Override
    protected Path manifestDirectory() {
        return super.manifestDirectory().resolve(DIRECTORY_NAME);
    }

    @Override
    protected String exitFailure(int exit) {
        return "The tests did not pass (status " + exit + "): see the reports in " + reportsDirectory();
    }

    @Override
    protected String[] runtimeScopes() {
        return new String[] {JavaScopes.PROVIDED, JavaScopes.COMPILE, JavaScopes.RUNTIME, JavaScopes.TEST};
    }

    @Override
    protected boolean requiresMainClass() {
        return false;
    }

    @Override
    protected String buildToolGoal() {
        return "test-compile";
    }

    @Override
    protected List<String> watchedRoots(MavenProject project) {
        List<String> roots = new ArrayList<>(super.watchedRoots(project));
        if (project.equals(runnableProject()) || reactorTestProjects().contains(project)) {
            roots.addAll(DevManifest.testSourceRoots(project));
        }
        return roots;
    }

    @Override
    protected DevManifest buildManifest(Path manifestDirectory) throws MojoExecutionException {
        DevManifest manifest = super.buildManifest(manifestDirectory);
        PluginParameterExpressionEvaluator evaluator = runnableEvaluator();
        List<File> testProcessorPath;
        try {
            DevManifest.ProcessorPaths paths = DevManifest.testProcessorPaths(runnableProject(), evaluator);
            testProcessorPath = paths.dependencies().isEmpty() ? List.of() : resolveProcessorPath(paths);
        } catch (Exception e) {
            throw new MojoExecutionException("Cannot resolve the test annotation processor path: " + e.getMessage(), e);
        }
        DevManifest.TestSettings settings = new DevManifest.TestSettings(runner, selection, initialRun, once,
            reportsDirectory(), htmlReportDirectory(), reportPath, filter(),
            configurationParameters == null ? Map.of() : new LinkedHashMap<>(configurationParameters));
        return manifest.withTests(runnableProject(), settings, testCompileClasspath(), testProcessorPath, reactorTestProjects(), evaluator);
    }

    /**
     * The reactor projects whose test-jar the tests depend on: the runtime classpath leaves the reactor projects'
     * artifacts out, and their test classes use their main classes, which are reloadable, so their test outputs are
     * reloadable as well, and their test sources are watched.
     */
    private List<MavenProject> reactorTestProjects() {
        List<MavenProject> projects = new ArrayList<>();
        for (org.apache.maven.artifact.Artifact artifact : runnableProject().getArtifacts()) {
            if (!"tests".equals(artifact.getClassifier()) && !"test-jar".equals(artifact.getType())) {
                continue;
            }
            for (MavenProject project : reactorDependencies()) {
                if (!project.equals(runnableProject()) && project.getGroupId().equals(artifact.getGroupId())
                    && project.getArtifactId().equals(artifact.getArtifactId()) && !projects.contains(project)) {
                    projects.add(project);
                }
            }
        }
        return projects;
    }

    /**
     * The reports directory, defaulted and resolved against the project whose tests run: an aggregator invocation
     * would evaluate the goal's defaults, and resolve a relative value, against the reactor root.
     */
    private Path reportsDirectory() {
        return projectDirectory(reportsDirectory, Path.of(runnableProject().getBuild().getDirectory(), "surefire-reports"));
    }

    private Path htmlReportDirectory() {
        return projectDirectory(htmlReportDirectory, Path.of(runnableProject().getBuild().getDirectory(), DevManifest.DIRECTORY_NAME, "test-report"));
    }

    private Path projectDirectory(String configured, Path defaultDirectory) {
        Path directory = configured == null || configured.isBlank() ? defaultDirectory : runnableProject().getBasedir().toPath().resolve(configured.trim());
        return directory.toAbsolutePath().normalize();
    }

    /**
     * The classpath the tests compile with, as {@code testCompile} sees it: the reloadable outputs first, the
     * runnable project's ahead of the reactor projects', where a changed class of a dependency is compiled to, the
     * test outputs of the reactor test-jars, and then the modules of every scope.
     */
    private List<File> testCompileClasspath() {
        List<File> classpath = new ArrayList<>();
        classpath.add(new File(runnableProject().getBuild().getOutputDirectory()));
        for (MavenProject project : reactorDependencies()) {
            File output = new File(project.getBuild().getOutputDirectory());
            if (!classpath.contains(output)) {
                classpath.add(output);
            }
        }
        for (MavenProject project : reactorTestProjects()) {
            File output = new File(project.getBuild().getTestOutputDirectory());
            if (!classpath.contains(output)) {
                classpath.add(output);
            }
        }
        classpath.addAll(files(runtimeDependencies()));
        return classpath;
    }

    private List<String> filter() {
        List<String> patterns = new ArrayList<>();
        if (filter != null) {
            for (String pattern : filter.split(",")) {
                if (!pattern.isBlank()) {
                    patterns.add(pattern.trim());
                }
            }
        }
        for (String pattern : surefireFilter(test)) {
            if (pattern.startsWith("!") || pattern.startsWith("%regex[")) {
                getLog().warn("The test pattern " + pattern + " is not supported by mn:test and is ignored");
            } else if (!patterns.contains(pattern)) {
                patterns.add(pattern);
            }
        }
        return patterns;
    }

    /**
     * Maps Surefire's {@code -Dtest} to the launcher's patterns: {@code Class#method} becomes {@code Class.method},
     * {@code Class#m1+m2} a pattern per method, and a path such as {@code **}{@code /FooTest.java} the class
     * pattern {@code FooTest}. Exclusions and {@code %regex[]} patterns are returned as they are.
     *
     * @param test the value of {@code -Dtest}
     * @return the patterns
     */
    static List<String> surefireFilter(String test) {
        List<String> patterns = new ArrayList<>();
        if (test == null || test.isBlank()) {
            return patterns;
        }
        for (String entry : test.split(",")) {
            String pattern = entry.trim();
            if (pattern.isEmpty()) {
                continue;
            }
            if (pattern.startsWith("!") || pattern.startsWith("%regex[")) {
                patterns.add(pattern);
                continue;
            }
            String methods = null;
            int hash = pattern.indexOf('#');
            if (hash >= 0) {
                methods = pattern.substring(hash + 1);
                pattern = pattern.substring(0, hash);
            }
            pattern = classPattern(pattern);
            if (methods == null || methods.isEmpty()) {
                patterns.add(pattern);
            } else {
                for (String method : methods.split("\\+")) {
                    if (!method.isBlank()) {
                        patterns.add(pattern + "." + method.trim());
                    }
                }
            }
        }
        return patterns;
    }

    /**
     * A Surefire class pattern as a class name, its path separators either slash: the leading {@code **}{@code /} and the {@code .java} or
     * {@code .class} extension dropped, and the path separators become dots.
     */
    private static String classPattern(String pattern) {
        // a Windows path as well
        String result = pattern.replace('\\', '/');
        while (result.startsWith("**/")) {
            result = result.substring(3);
        }
        if (result.endsWith(".java")) {
            result = result.substring(0, result.length() - ".java".length());
        } else if (result.endsWith(".class")) {
            result = result.substring(0, result.length() - ".class".length());
        }
        result = result.replace('/', '.');
        return result.isEmpty() ? "*" : result;
    }

    @Override
    protected List<Artifact> launcherArtifacts() {
        List<Artifact> artifacts = new ArrayList<>(super.launcherArtifacts());
        if (htmlReport) {
            artifacts.add(new DefaultArtifact(LAUNCHER_GROUP, TEST_REPORT_ARTIFACT, "jar", ""));
        }
        Artifact launcher = junitPlatformLauncher();
        if (launcher != null) {
            artifacts.add(launcher);
        }
        return artifacts;
    }

    /**
     * The JUnit Platform launcher, which the launcher's runner needs: none when the tests already depend on it,
     * else at the version of the project's JUnit Platform, else as the dependency management names it, else a
     * default.
     */
    private Artifact junitPlatformLauncher() {
        String platformVersion = null;
        for (Dependency dependency : runtimeDependencies()) {
            Artifact artifact = dependency.getArtifact();
            if (JUNIT_PLATFORM_GROUP.equals(artifact.getGroupId())) {
                if (JUNIT_PLATFORM_LAUNCHER.equals(artifact.getArtifactId())) {
                    return null;
                }
                if (JUNIT_PLATFORM_ENGINE.equals(artifact.getArtifactId())) {
                    platformVersion = artifact.getVersion();
                }
            }
        }
        if (platformVersion == null) {
            boolean managed = runnableProject().getDependencyManagement() != null && runnableProject().getDependencyManagement().getDependencies().stream()
                .anyMatch(managedDependency -> JUNIT_PLATFORM_GROUP.equals(managedDependency.getGroupId()) && JUNIT_PLATFORM_LAUNCHER.equals(managedDependency.getArtifactId()));
            platformVersion = managed ? "" : DEFAULT_JUNIT_PLATFORM_VERSION;
        }
        return new DefaultArtifact(JUNIT_PLATFORM_GROUP, JUNIT_PLATFORM_LAUNCHER, "jar", platformVersion);
    }
}
