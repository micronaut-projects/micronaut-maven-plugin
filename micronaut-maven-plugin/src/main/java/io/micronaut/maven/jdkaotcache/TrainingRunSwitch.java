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
package io.micronaut.maven.jdkaotcache;

import io.micronaut.core.annotation.Internal;
import org.apache.maven.artifact.Artifact;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * The training-run switch of Micronaut core (micronaut-projects/micronaut-core#13391): with
 * {@value #ENABLED_PROPERTY} set, {@code Micronaut.run} starts the application, sends the GET requests of
 * {@value #WARMUP_PATHS_PROPERTY} to it, stops it and exits with status 0, which ends the JDK AOT cache training
 * without a script.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public final class TrainingRunSwitch {

    /**
     * Turns the training run on.
     */
    public static final String ENABLED_PROPERTY = "micronaut.application.training.enabled";

    /**
     * The paths of the warm-up requests.
     */
    public static final String WARMUP_PATHS_PROPERTY = "micronaut.application.training.warmup.paths";

    private static final String MICRONAUT_GROUP_ID = "io.micronaut";
    private static final String CONTEXT_ARTIFACT_ID = "micronaut-context";
    private static final String HTTP_SERVER_ARTIFACT_ID = "micronaut-http-server";
    private static final String APPLICATION_CONFIGURATION_CLASS = "io/micronaut/runtime/ApplicationConfiguration.class";
    private static final String HTTP_SERVER_PACKAGE = "io/micronaut/http/server/";
    private static final String WARMUP_PREFIX = "micronaut.application.training.warmup";

    private TrainingRunSwitch() {
    }

    /**
     * Looks for the switch in the application's Micronaut JARs. The switch is there when {@code micronaut-context}
     * defines {@value #ENABLED_PROPERTY} and, if the training sends warm-up requests, when {@code micronaut-http-server}
     * has the warm-up.
     *
     * @param artifacts the resolved dependencies of the application
     * @param warmUp whether the training sends warm-up requests
     * @return whether the application's Micronaut version has the switch
     */
    public static boolean isAvailable(Collection<Artifact> artifacts, boolean warmUp) {
        Optional<File> context = findJar(artifacts, CONTEXT_ARTIFACT_ID);
        if (context.isEmpty() || !entryContains(context.get(), APPLICATION_CONFIGURATION_CLASS, ENABLED_PROPERTY)) {
            return false;
        }
        if (!warmUp) {
            return true;
        }
        Optional<File> httpServer = findJar(artifacts, HTTP_SERVER_ARTIFACT_ID);
        return httpServer.isPresent() && anyClassContains(httpServer.get(), WARMUP_PREFIX);
    }

    /**
     * @param paths the warm-up paths
     * @return the Java system properties that turn the switch on and set the warm-up paths
     */
    public static List<String> systemProperties(List<String> paths) {
        var properties = new ArrayList<String>(paths.size() + 1);
        properties.add("-D" + ENABLED_PROPERTY + "=true");
        for (int i = 0; i < paths.size(); i++) {
            properties.add("-D" + WARMUP_PATHS_PROPERTY + "[" + i + "]=" + paths.get(i));
        }
        return properties;
    }

    private static Optional<File> findJar(Collection<Artifact> artifacts, String artifactId) {
        return artifacts.stream()
            .filter(artifact -> MICRONAUT_GROUP_ID.equals(artifact.getGroupId()) && artifactId.equals(artifact.getArtifactId()))
            .map(Artifact::getFile)
            .filter(file -> file != null && file.isFile())
            .findFirst();
    }

    private static boolean entryContains(File jar, String entryName, String text) {
        try (var jarFile = new JarFile(jar)) {
            JarEntry entry = jarFile.getJarEntry(entryName);
            return entry != null && contains(jarFile, entry, text);
        } catch (IOException e) {
            return false;
        }
    }

    private static boolean anyClassContains(File jar, String text) {
        try (var jarFile = new JarFile(jar)) {
            var entries = jarFile.entries();
            while (entries.hasMoreElements()) {
                JarEntry entry = entries.nextElement();
                if (entry.getName().startsWith(HTTP_SERVER_PACKAGE) && entry.getName().endsWith(".class")
                    && contains(jarFile, entry, text)) {
                    return true;
                }
            }
            return false;
        } catch (IOException e) {
            return false;
        }
    }

    /**
     * Class files keep string constants as modified UTF-8, which is plain ASCII for property names, so decoding the
     * bytes as ISO-8859-1 keeps them as they are.
     */
    private static boolean contains(JarFile jarFile, JarEntry entry, String text) throws IOException {
        try (InputStream in = jarFile.getInputStream(entry)) {
            return new String(in.readAllBytes(), StandardCharsets.ISO_8859_1).contains(text);
        }
    }
}
