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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * The training-run switch of Micronaut core (micronaut-projects/micronaut-core#13391): with
 * {@value #ENABLED_PROPERTY} set, {@code Micronaut.run} ends the JDK AOT cache training without a script and exits
 * with status 0. {@value #MODE_PROPERTY} says how far it goes before that:
 * <ul>
 *     <li>{@code start}: it starts the application, sends the GET requests of {@value #WARMUP_PATHS_PROPERTY} to it
 *     and stops it.</li>
 *     <li>{@code load}: it loads the bean definitions and the classes they name, creates no bean and does not start
 *     the application.</li>
 * </ul>
 * Older versions with the switch have no mode and always start the application.
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
     * Selects the mode of the training run.
     */
    public static final String MODE_PROPERTY = "micronaut.application.training.mode";

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
     * Looks for the training mode in the application's {@code micronaut-context} JAR, which has it when it defines
     * {@value #MODE_PROPERTY} next to the switch.
     *
     * @param artifacts the resolved dependencies of the application
     * @return whether the application's Micronaut version can train without starting the application
     */
    public static boolean hasLoadMode(Collection<Artifact> artifacts) {
        return findJar(artifacts, CONTEXT_ARTIFACT_ID)
            .filter(context -> entryContains(context, APPLICATION_CONFIGURATION_CLASS, ENABLED_PROPERTY, MODE_PROPERTY))
            .isPresent();
    }

    /**
     * The mode is always passed, also when it is the default of Micronaut core, so that the training does not change
     * if that default does. A Micronaut version without the mode ignores the property.
     *
     * @param mode the mode of the training run
     * @param paths the warm-up paths, which only a run that starts the application sends
     * @return the Java system properties that turn the switch on, select the mode and set the warm-up paths
     */
    public static List<String> systemProperties(TrainingMode mode, List<String> paths) {
        var properties = new ArrayList<String>(paths.size() + 2);
        properties.add("-D" + ENABLED_PROPERTY + "=true");
        properties.add("-D" + MODE_PROPERTY + "=" + mode.id());
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

    private static boolean entryContains(File jar, String entryName, String... texts) {
        try (var jarFile = new JarFile(jar)) {
            JarEntry entry = jarFile.getJarEntry(entryName);
            if (entry == null) {
                return false;
            }
            String content = read(jarFile, entry);
            return Arrays.stream(texts).allMatch(content::contains);
        } catch (IOException _) {
            return false;
        }
    }

    private static boolean anyClassContains(File jar, String text) {
        try (var jarFile = new JarFile(jar)) {
            var entries = jarFile.entries();
            while (entries.hasMoreElements()) {
                JarEntry entry = entries.nextElement();
                if (entry.getName().startsWith(HTTP_SERVER_PACKAGE) && entry.getName().endsWith(".class")
                    && read(jarFile, entry).contains(text)) {
                    return true;
                }
            }
            return false;
        } catch (IOException _) {
            return false;
        }
    }

    /**
     * Class files keep string constants as modified UTF-8, which is plain ASCII for property names, so decoding the
     * bytes as ISO-8859-1 keeps them as they are.
     */
    private static String read(JarFile jarFile, JarEntry entry) throws IOException {
        try (InputStream in = jarFile.getInputStream(entry)) {
            return new String(in.readAllBytes(), StandardCharsets.ISO_8859_1);
        }
    }
}
