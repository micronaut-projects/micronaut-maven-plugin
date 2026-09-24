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

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

/**
 * Writes the files that a Dockerfile generated with {@code micronaut.docker.jdkAotCache} copies from the build
 * context: an application JAR, the class path argument file and the training script. A JDK AOT cache cannot record
 * classes from a non-empty directory on the class path, so the classes go into a JAR, listed after the dependencies,
 * as the classes directory is in the default Dockerfile. The dependencies are listed one by one, in the order of the
 * Maven class path, because the JDK leaves the expansion order of {@code *} entries unspecified.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public final class JdkAotCacheDockerContext {

    /**
     * The directory of the build context that holds the files.
     */
    public static final String CONTEXT_DIRECTORY = "jdk-aot-cache";

    /**
     * The application JAR.
     */
    public static final String APPLICATION_JAR = "application.jar";

    /**
     * The class path argument file, used as {@code -cp @classpath}.
     */
    public static final String CLASSPATH_FILE = "classpath";

    /**
     * The training script.
     */
    public static final String TRAINING_SCRIPT = "training.sh";

    /**
     * The working directory of the generated Dockerfile.
     */
    public static final String IMAGE_HOME = "/home/app";

    /**
     * Where the generated Dockerfile writes the cache.
     */
    public static final String IMAGE_CACHE_FILE = IMAGE_HOME + "/app.aot";

    /**
     * A fixed timestamp for the JAR entries, independent of the time zone, so that unchanged classes give an identical
     * JAR and Docker can reuse the cached training layer.
     */
    private static final LocalDateTime ENTRY_TIME = LocalDateTime.of(1980, 2, 1, 0, 0);

    private JdkAotCacheDockerContext() {
    }

    /**
     * Writes the application JAR, the class path argument file and the training script.
     *
     * @param buildDirectory the build context, usually {@code target}
     * @param classesDirectory the compiled classes and resources
     * @param dependencies the dependencies as paths in the image, in class path order
     * @throws IOException if a file cannot be written
     */
    public static void write(Path buildDirectory, Path classesDirectory, List<String> dependencies) throws IOException {
        Path context = buildDirectory.resolve(CONTEXT_DIRECTORY);
        Files.createDirectories(context);
        writeApplicationJar(classesDirectory, context.resolve(APPLICATION_JAR));
        var classpath = new ArrayList<>(dependencies);
        classpath.add(IMAGE_HOME + "/" + APPLICATION_JAR);
        Files.writeString(context.resolve(CLASSPATH_FILE), "\"" + String.join(":", classpath) + "\"\n", StandardCharsets.UTF_8);
        Files.writeString(context.resolve(TRAINING_SCRIPT), JdkAotCacheTraining.readScript(), StandardCharsets.UTF_8);
    }

    /**
     * Writes the content of a directory into a JAR, in a stable order and with fixed timestamps.
     *
     * @param classesDirectory the directory
     * @param jar the JAR to write
     * @throws IOException if the JAR cannot be written
     */
    static void writeApplicationJar(Path classesDirectory, Path jar) throws IOException {
        var manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        List<Path> paths;
        if (Files.isDirectory(classesDirectory)) {
            try (Stream<Path> walk = Files.walk(classesDirectory)) {
                paths = walk.filter(path -> !path.equals(classesDirectory)).sorted().toList();
            }
        } else {
            paths = List.of();
        }
        try (OutputStream out = Files.newOutputStream(jar);
             var jarOut = new JarOutputStream(out)) {
            jarOut.putNextEntry(entry("META-INF/"));
            jarOut.closeEntry();
            jarOut.putNextEntry(entry(JarFile.MANIFEST_NAME));
            manifest.write(jarOut);
            jarOut.closeEntry();
            for (Path path : paths) {
                String name = classesDirectory.relativize(path).toString().replace('\\', '/');
                if ("META-INF".equals(name) || JarFile.MANIFEST_NAME.equals(name)) {
                    continue;
                }
                boolean directory = Files.isDirectory(path);
                jarOut.putNextEntry(entry(directory ? name + "/" : name));
                if (!directory) {
                    Files.copy(path, jarOut);
                }
                jarOut.closeEntry();
            }
        }
    }

    private static ZipEntry entry(String name) {
        var entry = new ZipEntry(name);
        entry.setTimeLocal(ENTRY_TIME);
        return entry;
    }
}
