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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The class path that a {@link ClassPathTransformService} rewrote. Closing it gives the project's dependency
 * artifacts their original files back, so that the goals that run later, such as Failsafe's, use the original
 * JARs.
 *
 * @since 5.1.0
 */
public final class RewrittenClassPath implements AutoCloseable {

    private static final RewrittenClassPath NONE = new RewrittenClassPath(Map.of());

    private final Map<Path, Path> mapping;
    private final Map<Artifact, File> swapped = new LinkedHashMap<>();

    RewrittenClassPath(Map<Path, Path> mapping) {
        this.mapping = mapping;
    }

    /**
     * @return a class path that is not rewritten
     */
    public static RewrittenClassPath none() {
        return NONE;
    }

    /**
     * @param original a class path entry
     * @return what is packaged in its place: its rewritten copy, or the entry itself
     */
    public Path packaged(Path original) {
        return mapping.getOrDefault(original.toAbsolutePath(), original);
    }

    /**
     * Points the project's dependency artifacts to their rewritten copies, until this is closed.
     *
     * @param project the project
     * @return this
     */
    public RewrittenClassPath swapDependencies(MavenProject project) {
        for (Artifact artifact : project.getArtifacts()) {
            File file = artifact.getFile();
            if (file == null) {
                continue;
            }
            Path copy = mapping.get(file.toPath().toAbsolutePath());
            if (copy != null && !copy.equals(file.toPath().toAbsolutePath())) {
                swapped.putIfAbsent(artifact, file);
                artifact.setFile(copy.toFile());
            }
        }
        return this;
    }

    @Override
    public void close() {
        swapped.forEach(Artifact::setFile);
        swapped.clear();
    }
}
