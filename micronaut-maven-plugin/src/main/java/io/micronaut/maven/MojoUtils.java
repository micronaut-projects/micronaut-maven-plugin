/*
 * Copyright 2017-2022 original authors
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

import org.apache.commons.io.FilenameUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.micronaut.maven.AbstractDockerMojo.MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG;

/**
 * Utility methods for different mojos.
 */
public final class MojoUtils {

    public static final String THIS_PLUGIN = "io.micronaut.maven:micronaut-maven-plugin";

    private MojoUtils() {
    }

    public static String findJavaExecutable(ToolchainManager toolchainManager, MavenSession mavenSession) {
        return io.micronaut.maven.core.MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
    }

    public static List<String> computeNativeImageArgs(List<String> nativeImageBuildArgs, String baseImageRun, String argsFile) {
        var allNativeImageBuildArgs = new ArrayList<String>();
        if (nativeImageBuildArgs != null && !nativeImageBuildArgs.isEmpty()) {
            allNativeImageBuildArgs.addAll(nativeImageBuildArgs);
        }
        if (baseImageRun.contains("distroless") && !allNativeImageBuildArgs.contains(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG)) {
            allNativeImageBuildArgs.add(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG);
        }

        List<String> argsFileContent = parseNativeImageArgsFile(argsFile).toList();
        allNativeImageBuildArgs.addAll(argsFileContent);
        return allNativeImageBuildArgs;
    }

    private static Stream<String> parseNativeImageArgsFile(String argsFile) {
        return parseNativeImageArgsFile(Paths.get(FilenameUtils.separatorsToSystem(argsFile)));
    }

    private static Stream<String> parseNativeImageArgsFile(Path argsFilePath) {
        if (Files.exists(argsFilePath)) {
            List<String> args;
            try {
                args = Files.readAllLines(argsFilePath);
            } catch (IOException e) {
                throw new RuntimeException("Could not read the args file: " + argsFilePath, e);
            }
            if (args.contains("-cp")) {
                int cpPosition = args.indexOf("-cp");
                args.remove(cpPosition);
                args.remove(cpPosition);
            }

            return args.stream()
                .filter(arg -> !arg.startsWith("-H:Name"))
                .filter(arg -> !arg.startsWith("-H:Class"))
                .filter(arg -> !arg.startsWith("-H:Path"))
                .flatMap(arg -> {
                    if (arg.startsWith("@")) {
                        String fileName = arg.substring(1);
                        return parseNativeImageArgsFile(resolveNestedArgsFilePath(argsFilePath, fileName));
                    } else if (arg.startsWith("\\Q") && arg.endsWith("\\E")) {
                        return Stream.of(parseQuotedClasspathArg(arg));
                    } else if (arg.startsWith("-H:ConfigurationFileDirectories")) {
                        return Stream.of(parseConfigurationFilesDirectoriesArg(arg));
                    } else {
                        return Stream.of(arg);
                    }
                });
        } else {
            throw new RuntimeException("Unable to find args file: " + argsFilePath);
        }
    }

    private static Path resolveNestedArgsFilePath(Path argsFilePath, String fileName) {
        Path nestedArgsFilePath = Paths.get(FilenameUtils.separatorsToSystem(fileName)).normalize();
        if (nestedArgsFilePath.isAbsolute()) {
            return nestedArgsFilePath;
        }
        Path parent = argsFilePath.getParent();
        if (parent != null) {
            Path resolved = parent.resolve(nestedArgsFilePath).normalize();
            if (Files.exists(resolved)) {
                return resolved;
            }
        }
        return nestedArgsFilePath;
    }

    private static String parseQuotedClasspathArg(String arg) {
        String quotedPath = arg.substring(2, arg.length() - 2);
        String normalizedPath = FilenameUtils.separatorsToUnix(quotedPath);
        String fileName = FilenameUtils.getName(quotedPath);
        String layerDirectory = normalizedPath.contains("-SNAPSHOT/") || fileName.contains("SNAPSHOT")
            ? "snapshot"
            : "release";
        return "\\Q/home/app/libs/" + layerDirectory + "/" + fileName + "\\E";
    }

    static String parseConfigurationFilesDirectoriesArg(String arg) {
        String[] split = arg.split("=");
        String[] directories = split[1].split(",");
        String separator = "/";
        if (arg.contains("generateResourceConfig") || arg.contains("generateTestResourceConfig")) {
            return Stream.of(directories)
                .map(FilenameUtils::separatorsToUnix)
                .map(directory -> {
                    String[] splitDirectory = directory.split(separator);
                    return "/home/app/" + splitDirectory[splitDirectory.length - 1];
                })
                .collect(Collectors.joining(","))
                .transform(s -> "-H:ConfigurationFileDirectories=" + s);
        } else {
            return Stream.of(directories)
                .map(FilenameUtils::separatorsToUnix)
                .map(directory -> {
                    String[] splitDirectory = directory.split(separator);
                    String last4Directories = splitDirectory[splitDirectory.length - 4] + separator +
                        splitDirectory[splitDirectory.length - 3] + separator +
                        splitDirectory[splitDirectory.length - 2] + separator +
                        splitDirectory[splitDirectory.length - 1];
                    return "/home/app/graalvm-reachability-metadata/" + last4Directories;
                })
                .collect(Collectors.joining(","))
                .transform(s -> "-H:ConfigurationFileDirectories=" + s);
        }
    }

    /**
     * Checks if the project has the Micronaut Maven plugin defined.
     *
     * @param project the Maven project
     * @return true if the project has the Micronaut Maven plugin defined
     */
    public static boolean hasMicronautMavenPlugin(MavenProject project) {
        return io.micronaut.maven.core.MojoUtils.hasMicronautMavenPlugin(project);
    }
}
