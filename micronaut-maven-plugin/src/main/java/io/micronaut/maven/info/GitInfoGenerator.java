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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;

/**
 * Generates git metadata properties.
 *
 * @author Micronaut Authors
 * @since 5.0.1
 */
final class GitInfoGenerator {

    private static final Duration COMMAND_TIMEOUT = Duration.ofSeconds(10);
    private static final String CONFIG = "config";
    private static final String CONFIG_GET = "--get";
    private static final String REV_PARSE = "rev-parse";

    private final Path workingDirectory;
    private final boolean includeDirty;
    private final boolean includeRemoteUrl;
    private final boolean includeUser;

    GitInfoGenerator(Path workingDirectory, boolean includeDirty, boolean includeRemoteUrl, boolean includeUser) {
        this.workingDirectory = workingDirectory;
        this.includeDirty = includeDirty;
        this.includeRemoteUrl = includeRemoteUrl;
        this.includeUser = includeUser;
    }

    Result generate(Map<String, String> additionalProperties) {
        CommandResult root = runGit(REV_PARSE, "--show-toplevel");
        if (!root.isSuccess()) {
            return Result.unavailable(root.message());
        }
        CommandResult commitId = runGit(REV_PARSE, "HEAD");
        if (!commitId.isSuccess()) {
            return Result.unavailable(commitId.message());
        }
        var properties = new TreeMap<String, String>();
        properties.put("git.commit.id", commitId.output());
        putIfPresent(properties, "git.commit.id.abbrev", runGit(REV_PARSE, "--short", "HEAD"));
        putIfPresent(properties, "git.commit.time", runGit("show", "-s", "--format=%cI", "HEAD"));
        putBranch(properties);
        if (includeDirty) {
            CommandResult status = runGit("status", "--porcelain");
            if (status.isSuccess()) {
                properties.put("git.dirty", Boolean.toString(!status.output().isBlank()));
            }
        }
        if (includeRemoteUrl) {
            putIfPresent(properties, "git.remote.origin.url", runGit(CONFIG, CONFIG_GET, "remote.origin.url"));
        }
        if (includeUser) {
            putIfPresent(properties, "git.build.user.name", runGit(CONFIG, CONFIG_GET, "user.name"));
            putIfPresent(properties, "git.build.user.email", runGit(CONFIG, CONFIG_GET, "user.email"));
        }
        if (additionalProperties != null) {
            additionalProperties.forEach((key, value) -> putIfNotBlank(properties, key, value));
        }
        return Result.available(properties);
    }

    private void putBranch(Map<String, String> properties) {
        CommandResult branch = runGit(REV_PARSE, "--abbrev-ref", "HEAD");
        if (branch.isSuccess() && !"HEAD".equals(branch.output())) {
            properties.put("git.branch", branch.output());
        }
    }

    private void putIfPresent(Map<String, String> properties, String key, CommandResult result) {
        if (result.isSuccess() && !result.output().isBlank()) {
            properties.put(key, result.output());
        }
    }

    private CommandResult runGit(String... arguments) {
        var command = new java.util.ArrayList<String>();
        command.add("git");
        command.addAll(List.of(arguments));
        Process process = null;
        try {
            process = new ProcessBuilder(command)
                .directory(workingDirectory.toFile())
                .redirectErrorStream(true)
                .start();
            boolean finished = process.waitFor(COMMAND_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return CommandResult.failure("git command timed out: " + String.join(" ", command));
            }
            String output = read(process).trim();
            if (process.exitValue() != 0) {
                return CommandResult.failure(output.isBlank() ? "git command failed" : output);
            }
            return CommandResult.success(output);
        } catch (IOException e) {
            return CommandResult.failure(e.toString());
        } catch (InterruptedException _) {
            Thread.currentThread().interrupt();
            return CommandResult.failure("git command was interrupted");
        } finally {
            if (process != null) {
                process.destroy();
            }
        }
    }

    private String read(Process process) throws IOException {
        var output = new ByteArrayOutputStream();
        process.getInputStream().transferTo(output);
        return output.toString(StandardCharsets.UTF_8);
    }

    private void putIfNotBlank(Map<String, String> properties, String key, String value) {
        if (key != null && !key.isBlank() && value != null && !value.isBlank()) {
            properties.put(key, value);
        }
    }

    record Result(Map<String, String> properties, String message) {

        static Result available(Map<String, String> properties) {
            return new Result(properties, null);
        }

        static Result unavailable(String message) {
            return new Result(Map.of(), message);
        }
    }

    private record CommandResult(boolean isSuccess, String output, String message) {

        static CommandResult success(String output) {
            return new CommandResult(true, output, null);
        }

        static CommandResult failure(String message) {
            return new CommandResult(false, null, message);
        }
    }
}
