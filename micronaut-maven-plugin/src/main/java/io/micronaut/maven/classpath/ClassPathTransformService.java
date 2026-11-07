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
package io.micronaut.maven.classpath;

import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.services.DependencyResolutionService;
import javax.inject.Inject;
import javax.inject.Singleton;
import org.apache.commons.io.FileUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.resolution.DependencyResolutionException;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Rewrites the packaged class path with Micronaut AOT's {@code micronaut-aot-bytecode}: the application's output
 * first, then the compile and runtime scope dependencies. The transform runs in a JVM of the {@code jdk} toolchain,
 * or of Maven's JVM without one, because its output depends on the JDK build.
 *
 * @since 5.1.0
 */
@Singleton
public class ClassPathTransformService {

    /** The default version of {@code micronaut-aot-bytecode}. */
    public static final String DEFAULT_VERSION = "3.2.0-M1";

    static final String CLASS_PATH_FILE = "class-path.txt";
    static final String REPORT_FILE = "report.txt";

    private static final Set<String> IMAGE_SCOPES = Set.of(Artifact.SCOPE_COMPILE, Artifact.SCOPE_RUNTIME);

    private final MavenSession mavenSession;
    private final DependencyResolutionService dependencyResolutionService;
    private final ToolchainManager toolchainManager;

    @Inject
    public ClassPathTransformService(MavenSession mavenSession,
                                     DependencyResolutionService dependencyResolutionService,
                                     ToolchainManager toolchainManager) {
        this.mavenSession = mavenSession;
        this.dependencyResolutionService = dependencyResolutionService;
        this.toolchainManager = toolchainManager;
    }

    /**
     * Rewrites the class path of a project, when a step is enabled.
     *
     * @param project the project
     * @param options the options
     * @param application the application's output: its classes directory or its JAR
     * @param name the name of the output directory, under {@code target/class-path-transform}
     * @param log the log
     * @return the rewritten class path, or one that is not rewritten when no step is enabled
     * @throws MojoExecutionException if the transform fails
     */
    public RewrittenClassPath rewrite(MavenProject project, ClassPathTransformOptions options, Path application,
                                      String name, Log log) throws MojoExecutionException {
        if (!options.enabled()) {
            return RewrittenClassPath.none();
        }
        Set<String> reactor = mavenSession.getProjects().stream()
            .map(p -> p.getGroupId() + ":" + p.getArtifactId())
            .collect(Collectors.toSet());
        List<Path> classPath = new ArrayList<>();
        List<Path> thirdParty = new ArrayList<>();
        if (Files.exists(application)) {
            classPath.add(application.toAbsolutePath());
        }
        for (Artifact artifact : project.getArtifacts()) {
            File file = artifact.getFile();
            if (IMAGE_SCOPES.contains(artifact.getScope()) && file != null && file.exists()
                && !classPath.contains(file.toPath().toAbsolutePath())) {
                Path entry = file.toPath().toAbsolutePath();
                classPath.add(entry);
                if (file.isFile() && !reactor.contains(artifact.getGroupId() + ":" + artifact.getArtifactId())) {
                    thirdParty.add(entry);
                }
            }
        }
        Path output = Path.of(project.getBuild().getDirectory(), "class-path-transform", name).toAbsolutePath();
        try {
            FileUtils.deleteDirectory(output.toFile());
            Files.createDirectories(output);
            List<String> request = new ArrayList<>();
            request.add(ClassPathTransformLauncher.OUTPUT + "\t" + output);
            request.add(ClassPathTransformLauncher.DESUGAR + "\t" + options.desugarLambdas());
            request.add(ClassPathTransformLauncher.STRIP + "\t" + options.stripLocalVariables());
            classPath.forEach(entry -> request.add(ClassPathTransformLauncher.ENTRY + "\t" + entry));
            thirdParty.forEach(jar -> request.add(ClassPathTransformLauncher.STRIPPED + "\t" + jar));
            Path requestFile = output.resolve("request.txt");
            Files.write(requestFile, request, StandardCharsets.UTF_8);
            fork(launcherClassPath(options.version()), requestFile, log);
            return new RewrittenClassPath(readMapping(output.resolve(CLASS_PATH_FILE)));
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to rewrite the class path: " + e.getMessage(), e);
        }
    }

    private String launcherClassPath(String version) throws MojoExecutionException {
        try {
            Stream<org.eclipse.aether.artifact.Artifact> bytecode = Stream.of(
                new DefaultArtifact("io.micronaut.aot:micronaut-aot-bytecode:" + version));
            List<String> classPath = new ArrayList<>();
            classPath.add(Path.of(ClassPathTransformLauncher.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString());
            classPath.addAll(DependencyResolutionService.toClasspath(dependencyResolutionService.artifactResultsFor(bytecode, false)));
            return String.join(File.pathSeparator, classPath);
        } catch (DependencyResolutionException e) {
            throw new MojoExecutionException("Unable to resolve io.micronaut.aot:micronaut-aot-bytecode:" + version, e);
        } catch (URISyntaxException e) {
            throw new MojoExecutionException("Unable to locate the Micronaut Maven Plugin", e);
        }
    }

    private void fork(String classPath, Path requestFile, Log log) throws IOException, MojoExecutionException {
        String java = MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
        Process process = new ProcessBuilder(java, "-cp", classPath, ClassPathTransformLauncher.class.getName(),
            requestFile.toString())
            .redirectErrorStream(true)
            .start();
        List<String> output = new ArrayList<>();
        try (var reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                output.add(line);
                if (line.startsWith(ClassPathTransformLauncher.WARNING_PREFIX)) {
                    log.warn(line.substring(ClassPathTransformLauncher.WARNING_PREFIX.length()));
                } else {
                    log.info(line);
                }
            }
        }
        try {
            int exitCode = process.waitFor();
            if (exitCode != 0) {
                throw new MojoExecutionException("The class path transform failed with exit code " + exitCode
                    + ". It needs a jdk toolchain, or a Maven JVM, of Java 25 or later. Output:\n"
                    + String.join("\n", output));
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new MojoExecutionException("Interrupted while rewriting the class path", e);
        }
    }

    private static Map<Path, Path> readMapping(Path file) throws IOException {
        Map<Path, Path> mapping = new LinkedHashMap<>();
        for (String line : Files.readAllLines(file, StandardCharsets.UTF_8)) {
            int tab = line.indexOf('\t');
            if (tab > 0) {
                mapping.put(Path.of(line.substring(0, tab)), Path.of(line.substring(tab + 1)));
            }
        }
        return mapping;
    }
}
