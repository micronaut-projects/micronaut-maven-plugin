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
package io.micronaut.maven.info;

import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GenerateInfoMojoTest {

    @Test
    void generatesBuildInfoWhenGitInfoIsDisabled(@TempDir Path tempDir) throws Exception {
        GenerateInfoMojo mojo = mojo(tempDir);
        File buildInfo = tempDir.resolve("classes/META-INF/build-info.properties").toFile();
        File gitInfo = tempDir.resolve("classes/git.properties").toFile();
        set(mojo, "buildOutputFile", buildInfo);
        set(mojo, "gitOutputFile", gitInfo);
        set(mojo, "buildEnabled", true);
        set(mojo, "gitEnabled", false);

        mojo.execute();

        Properties properties = new Properties();
        try (var reader = Files.newBufferedReader(buildInfo.toPath())) {
            properties.load(reader);
        }
        assertEquals("demo", properties.getProperty("build.artifact"));
        assertEquals("1.0.0", properties.getProperty("build.version"));
        assertTrue(properties.containsKey("build.time"));
        assertFalse(gitInfo.exists());
    }

    @Test
    void skipDoesNotWriteInfoFiles(@TempDir Path tempDir) throws Exception {
        GenerateInfoMojo mojo = mojo(tempDir);
        File buildInfo = tempDir.resolve("classes/META-INF/build-info.properties").toFile();
        set(mojo, "buildOutputFile", buildInfo);
        set(mojo, "skip", true);
        set(mojo, "buildEnabled", true);
        set(mojo, "gitEnabled", true);

        mojo.execute();

        assertFalse(buildInfo.exists());
    }

    @Test
    void invalidBuildTimeFailsTheMojo(@TempDir Path tempDir) throws Exception {
        GenerateInfoMojo mojo = mojo(tempDir);
        set(mojo, "time", "not-a-timestamp");
        set(mojo, "buildEnabled", true);
        set(mojo, "gitEnabled", false);
        set(mojo, "buildOutputFile", tempDir.resolve("build-info.properties").toFile());

        MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);
        assertEquals("Error resolving Micronaut build info timestamp", exception.getMessage());
    }

    @Test
    void failOnNoGitTurnsUnavailableGitMetadataIntoMojoFailure(@TempDir Path tempDir) throws Exception {
        GenerateInfoMojo mojo = mojo(tempDir);
        set(mojo, "buildEnabled", false);
        set(mojo, "gitEnabled", true);
        set(mojo, "failOnNoGit", true);
        set(mojo, "gitOutputFile", tempDir.resolve("git.properties").toFile());

        MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);
        assertTrue(exception.getMessage().startsWith("Git metadata is unavailable:"));
    }

    @Test
    void missingGitMetadataIsSkippedWhenFailureIsDisabled(@TempDir Path tempDir) throws Exception {
        GenerateInfoMojo mojo = mojo(tempDir);
        File gitInfo = tempDir.resolve("classes/git.properties").toFile();
        set(mojo, "buildEnabled", false);
        set(mojo, "gitEnabled", true);
        set(mojo, "failOnNoGit", false);
        set(mojo, "gitOutputFile", gitInfo);

        mojo.execute();

        assertFalse(gitInfo.exists());
    }

    @Test
    void generatesGitInfoWhenGitRepositoryIsAvailable(@TempDir Path tempDir) throws Exception {
        git(tempDir, "init");
        git(tempDir, "config", "user.email", "dev@example.com");
        git(tempDir, "config", "user.name", "Dev User");
        Files.writeString(tempDir.resolve("README.md"), "test");
        git(tempDir, "add", "README.md");
        git(tempDir, "commit", "-m", "Initial commit");

        GenerateInfoMojo mojo = mojo(tempDir);
        File gitInfo = tempDir.resolve("classes/git.properties").toFile();
        set(mojo, "buildEnabled", false);
        set(mojo, "gitEnabled", true);
        set(mojo, "gitOutputFile", gitInfo);
        set(mojo, "includeDirty", true);
        set(mojo, "includeRemoteUrl", false);
        set(mojo, "includeUser", false);

        mojo.execute();

        Properties properties = new Properties();
        try (var reader = Files.newBufferedReader(gitInfo.toPath())) {
            properties.load(reader);
        }
        assertEquals(gitOutput(tempDir, "rev-parse", "HEAD"), properties.getProperty("git.commit.id"));
        assertTrue(properties.containsKey("git.commit.time"));
    }

    private GenerateInfoMojo mojo(Path tempDir) throws Exception {
        var mojo = new GenerateInfoMojo();
        mojo.setLog(new SystemStreamLog());
        set(mojo, "project", project(tempDir));
        return mojo;
    }

    private MavenProject project(Path baseDir) {
        var project = new MavenProject();
        project.setGroupId("io.micronaut.test");
        project.setArtifactId("demo");
        project.setName("Demo");
        project.setVersion("1.0.0");
        project.setFile(baseDir.resolve("pom.xml").toFile());
        var build = new Build();
        build.setOutputDirectory(baseDir.resolve("classes").toString());
        project.setBuild(build);
        return project;
    }

    private void set(GenerateInfoMojo mojo, String fieldName, Object value) throws ReflectiveOperationException {
        Field field = GenerateInfoMojo.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(mojo, value);
    }

    private void git(Path directory, String... args) throws IOException, InterruptedException {
        Process process = new ProcessBuilder(command(args))
            .directory(directory.toFile())
            .redirectErrorStream(true)
            .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assertEquals(0, process.waitFor(), output);
    }

    private String gitOutput(Path directory, String... args) throws IOException, InterruptedException {
        Process process = new ProcessBuilder(command(args))
            .directory(directory.toFile())
            .redirectErrorStream(true)
            .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        assertEquals(0, process.waitFor(), output);
        return output;
    }

    private String[] command(String... args) {
        String[] command = new String[args.length + 1];
        command[0] = "git";
        System.arraycopy(args, 0, command, 1, args.length);
        return command;
    }
}
