/*
 * Copyright 2017-2024 original authors
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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Delete generated args file to prepare for the next build.
 * @author Lahoucine EL ADDALI
 * @since 4.6.2
 */
@Mojo(name = CleanArgsFileMojo.MOJO_NAME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE, threadSafe = true, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class CleanArgsFileMojo extends AbstractMicronautMojo {

    public static final String MOJO_NAME = "clean-args-file";

    /**
     * Reference to the Maven project on which the plugin is invoked.
     */
    private final MavenProject mavenProject;

    @Inject
    public CleanArgsFileMojo(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        List<File> existingArgsFiles = getAllArgsFile();
        try {
            deleteArgsFiles(existingArgsFiles);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Get the list of all argument files.
     *
     * @return the list of argument files
     */
    public List<File> getAllArgsFile() {
        File target = new File(this.mavenProject.getBuild().getDirectory());
        if (!target.exists()) {
            return new ArrayList<>();
        }
        return Arrays.stream(Objects.requireNonNull(target.listFiles())).
                filter(f -> f.getName().startsWith("native-image") && f.getName().endsWith(".args")).
                collect(Collectors.toList());
    }

    /**
     * Delete all argument files to prepare for next build.
     *
     * @param argsFiles the Maven dependencies
     * @throws IOException when the argument file is not found
     */
    public void deleteArgsFiles(List<File> argsFiles) throws IOException {
        for (File argFile: argsFiles) {
            if (argFile.exists()) {
                Files.delete(Paths.get(argFile.toURI()));
            }
        }
    }
}
