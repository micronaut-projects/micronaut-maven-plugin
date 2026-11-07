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

import io.micronaut.aot.bytecode.ClassPathTransform;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Runs Micronaut AOT's {@link ClassPathTransform} in a JVM of the project's toolchain, which has no main class of
 * its own. It reads a request file, writes {@value ClassPathTransformService#CLASS_PATH_FILE} and
 * {@value ClassPathTransformService#REPORT_FILE} to the output directory, and prints the lines to log.
 *
 * <p>The request file has one {@code key<TAB>value} pair per line: {@code output}, {@code desugar} and
 * {@code strip} once, and one {@code entry} per class path entry, in order, and one {@code stripped} per
 * third-party JAR.</p>
 */
public final class ClassPathTransformLauncher {

    static final String OUTPUT = "output";
    static final String DESUGAR = "desugar";
    static final String STRIP = "strip";
    static final String ENTRY = "entry";
    static final String STRIPPED = "stripped";
    static final String WARNING_PREFIX = "WARNING ";

    /** The standard output, which the plugin reads and logs. */
    private static final PrintStream OUT = new PrintStream(new FileOutputStream(FileDescriptor.out), true,
        StandardCharsets.UTF_8);

    private ClassPathTransformLauncher() {
    }

    /**
     * Runs the transform.
     *
     * @param args the request file
     * @throws IOException if an entry cannot be read or a copy cannot be written
     */
    public static void main(String[] args) throws IOException {
        Path output = null;
        boolean desugar = false;
        boolean strip = false;
        List<Path> classPath = new ArrayList<>();
        List<Path> stripped = new ArrayList<>();
        for (String line : Files.readAllLines(Path.of(args[0]), StandardCharsets.UTF_8)) {
            int tab = line.indexOf('\t');
            if (tab < 0) {
                continue;
            }
            String value = line.substring(tab + 1);
            switch (line.substring(0, tab)) {
                case OUTPUT -> output = Path.of(value);
                case DESUGAR -> desugar = Boolean.parseBoolean(value);
                case STRIP -> strip = Boolean.parseBoolean(value);
                case ENTRY -> classPath.add(Path.of(value));
                case STRIPPED -> stripped.add(Path.of(value));
                default -> throw new IllegalArgumentException("Unknown request key in " + line);
            }
        }
        if (output == null) {
            throw new IllegalArgumentException("The request has no output directory");
        }
        ClassPathTransform.Result result = ClassPathTransform.run(ClassPathTransform.Request.builder()
            .classPath(classPath)
            .outputDirectory(output.resolve("copies"))
            .desugarLambdas(desugar)
            .stripLocalVariables(strip ? stripped : List.of())
            .build());
        List<String> mapping = new ArrayList<>(classPath.size());
        for (int i = 0; i < classPath.size(); i++) {
            mapping.add(classPath.get(i).toAbsolutePath() + "\t" + result.classPath().get(i).toAbsolutePath());
        }
        List<String> report = new ArrayList<>();
        for (String line : result.summary().split("\n")) {
            if (!line.isBlank()) {
                OUT.println(line);
                report.add(line);
            }
        }
        for (String warning : result.warnings()) {
            OUT.println(WARNING_PREFIX + warning);
            report.add(WARNING_PREFIX + warning);
        }
        for (ClassPathTransform.Result.Entry entry : result.entries()) {
            report.add(entry.path().getFileName() + ": " + entry.sitesRewritten() + " lambda sites rewritten, "
                + entry.classesGenerated() + " classes generated, " + entry.classesStripped() + " classes stripped, "
                + entry.bytesSaved() + " bytes saved" + entry.kept().map(reason -> ", kept: " + reason).orElse(""));
            report.addAll(entry.notes());
        }
        Files.write(output.resolve(ClassPathTransformService.CLASS_PATH_FILE), mapping, StandardCharsets.UTF_8);
        Files.write(output.resolve(ClassPathTransformService.REPORT_FILE), report, StandardCharsets.UTF_8);
    }
}
