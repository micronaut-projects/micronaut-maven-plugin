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
package io.micronaut.maven.logback;

import io.micronaut.aot.logback.LogbackPrecompiler;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.CRC32;

/**
 * <p>Compiles {@code logback.xml} into a Logback {@code Configurator}, with Micronaut AOT's Logback precompiler,
 * and writes it into the build output directory, so that every packaging built from it ships it. It is off by
 * default: set {@code micronaut.logback.precompile} to {@code true}. {@code logback.xml} stays in place.</p>
 * <p>The goal records what it writes, with each file's size and CRC-32, and deletes those files before it runs
 * again, or when it is turned off, as long as they are unchanged.</p>
 * <p><strong>WARNING</strong>: this goal is not intended to be executed directly: the packagings bind it to
 * {@code prepare-package}, after {@code aot-analysis}.</p>
 *
 * @since 5.1.0
 */
@Mojo(name = PrecompileLogbackMojo.NAME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class PrecompileLogbackMojo extends AbstractMojo {

    public static final String NAME = "precompile-logback";

    static final String RECORD_FILE = "micronaut-logback/precompiled.txt";

    /**
     * Whether {@code logback.xml} is compiled into a Logback {@code Configurator} at build time.
     */
    @Parameter(property = "micronaut.logback.precompile", defaultValue = "false")
    private boolean precompile;

    private final MavenProject mavenProject;

    @Inject
    public PrecompileLogbackMojo(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
    }

    @Override
    public void execute() throws MojoExecutionException {
        Path classes = Path.of(mavenProject.getBuild().getOutputDirectory());
        Path record = Path.of(mavenProject.getBuild().getDirectory()).resolve(RECORD_FILE);
        try {
            removeEarlierOutput(classes, record);
            if (!precompile || !Files.isDirectory(classes)) {
                return;
            }
            Integer release = targetRelease();
            if (release == null) {
                getLog().info("No Logback configuration was precompiled because the target Java release is unknown: "
                    + "set maven.compiler.release");
                return;
            }
            LogbackPrecompiler.Result result = LogbackPrecompiler.precompile(LogbackPrecompiler.Request.builder()
                .applicationOutput(List.of(classes))
                .runtimeClasspath(runtimeClasspath())
                .targetRelease(release)
                .build());
            if (result.status() == LogbackPrecompiler.Status.FAILED) {
                getLog().warn(result.message());
            } else {
                getLog().info(result.message());
            }
            result.warnings().forEach(getLog()::warn);
            write(classes, record, result.entries());
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to precompile logback.xml", e);
        }
    }

    private Integer targetRelease() {
        Properties properties = mavenProject.getProperties();
        for (String key : List.of("maven.compiler.release", "maven.compiler.target")) {
            String value = properties.getProperty(key);
            if (value != null && !value.isBlank()) {
                String version = value.trim();
                try {
                    return Integer.parseInt(version.startsWith("1.") ? version.substring(2) : version);
                } catch (NumberFormatException e) {
                    getLog().debug("Ignoring " + key + "=" + value);
                }
            }
        }
        return null;
    }

    private List<Path> runtimeClasspath() {
        List<Path> entries = new ArrayList<>();
        for (Artifact artifact : mavenProject.getArtifacts()) {
            File file = artifact.getFile();
            if (artifact.getArtifactHandler().isAddedToClasspath() && file != null && file.exists()
                && (Artifact.SCOPE_COMPILE.equals(artifact.getScope()) || Artifact.SCOPE_RUNTIME.equals(artifact.getScope()))) {
                entries.add(file.toPath());
            }
        }
        return entries;
    }

    private static void write(Path classes, Path record, Map<String, byte[]> entries) throws IOException {
        if (entries.isEmpty()) {
            return;
        }
        List<String> lines = new ArrayList<>();
        for (Map.Entry<String, byte[]> entry : entries.entrySet()) {
            Path file = classes.resolve(entry.getKey()).normalize();
            if (!file.startsWith(classes)) {
                throw new IOException("Unexpected entry generated by the Logback precompiler: " + entry.getKey());
            }
            Files.createDirectories(file.getParent());
            Files.write(file, entry.getValue());
            lines.add(entry.getKey() + '\t' + entry.getValue().length + '\t' + crc(entry.getValue()));
        }
        Files.createDirectories(record.getParent());
        Files.write(record, lines, StandardCharsets.UTF_8);
    }

    /**
     * Deletes the files of an earlier run that are still as it wrote them. A file that changed since, such as the
     * service file that {@code aot-analysis} writes at the same path, is left alone.
     */
    private void removeEarlierOutput(Path classes, Path record) throws IOException {
        if (!Files.isRegularFile(record)) {
            return;
        }
        for (String line : Files.readAllLines(record, StandardCharsets.UTF_8)) {
            String[] parts = line.split("\t");
            if (parts.length != 3) {
                continue;
            }
            Path file = classes.resolve(parts[0]).normalize();
            if (file.startsWith(classes) && Files.isRegularFile(file)) {
                byte[] content = Files.readAllBytes(file);
                if (String.valueOf(content.length).equals(parts[1]) && String.valueOf(crc(content)).equals(parts[2])) {
                    Files.delete(file);
                } else {
                    getLog().debug("Keeping " + file + ", which changed since it was precompiled");
                }
            }
        }
        Files.delete(record);
    }

    private static long crc(byte[] content) {
        var crc = new CRC32();
        crc.update(content);
        return crc.getValue();
    }
}
