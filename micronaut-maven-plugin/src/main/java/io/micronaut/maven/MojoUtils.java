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

import org.apache.maven.execution.MavenSession;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.Os;

import java.io.File;
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

    private static final String JAVA = "java";

    private MojoUtils() {
    }

    public static String findJavaExecutable(ToolchainManager toolchainManager, MavenSession mavenSession) {
        String executable;
        Toolchain toolchain = toolchainManager.getToolchainFromBuildContext("jdk", mavenSession);
        if (toolchain != null) {
            executable = toolchain.findTool(JAVA);
        } else {
            File javaBinariesDir = new File(new File(System.getProperty("java.home")), "bin");
            if (Os.isFamily(Os.FAMILY_UNIX)) {
                executable = new File(javaBinariesDir, JAVA).getAbsolutePath();
            } else if (Os.isFamily(Os.FAMILY_WINDOWS)) {
                executable = new File(javaBinariesDir, "java.exe").getAbsolutePath();
            } else {
                executable = JAVA;
            }
        }
        return executable;
    }

    public static List<String> computeNativeImageArgs(List<String> nativeImageBuildArgs, String baseImageRun, String argsFile) {
        List<String> allNativeImageBuildArgs = new ArrayList<>();
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
        Path argsFilePath = Paths.get(argsFile);
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
//                    .filter(arg -> !arg.startsWith("-H:ConfigurationFileDirectories"))
                    .flatMap(arg -> {
                        if (arg.startsWith("@")) {
                            String fileName = arg.substring(1);
                            return parseNativeImageArgsFile(fileName);
                        } else if (arg.startsWith("\\Q") && arg.endsWith("\\E")) {
                            // start the search at length - 3 to skip \Q or \E at the end
                            int lastIndexOfSlash = arg.lastIndexOf(File.separator, arg.length() - 3);
                            return Stream.of("\\Q/home/app/libs/" + arg.substring(lastIndexOfSlash + 1));
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

    static String parseConfigurationFilesDirectoriesArg(String arg) {
        String[] split = arg.split("=");
        String[] directories = split[1].split(",");
        if (arg.contains("generateResourceConfig") || arg.contains("generateTestResourceConfig")) {
            return Stream.of(directories)
                    .map(directory -> {
                        String[] splitDirectory = directory.split("/");
                        return "/home/app/" + splitDirectory[splitDirectory.length - 1];
                    })
                    .collect(Collectors.joining(","))
                    .transform(s -> "-H:ConfigurationFileDirectories=" + s);
        } else {
            return Stream.of(directories)
                    .map(directory -> {
                        String[] splitDirectory = directory.split("/");
                        String last4Directories = splitDirectory[splitDirectory.length - 4] + "/" +
                                splitDirectory[splitDirectory.length - 3] + "/" +
                                splitDirectory[splitDirectory.length - 2] + "/" +
                                splitDirectory[splitDirectory.length - 1];
                        return "/home/app/graalvm-reachability-metadata/" + last4Directories;
                    })
                    .collect(Collectors.joining(","))
                    .transform(s -> "-H:ConfigurationFileDirectories=" + s);
        }
    }
}
