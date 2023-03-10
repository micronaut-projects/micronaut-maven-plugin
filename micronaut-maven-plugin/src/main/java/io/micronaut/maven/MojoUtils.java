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

    public static List<String> computeNativeImageArgs(List<String> nativeImageBuildArgs, String baseImageRun, String argsFile) throws IOException {
        List<String> allNativeImageBuildArgs = new ArrayList<>();
        if (nativeImageBuildArgs != null && !nativeImageBuildArgs.isEmpty()) {
            allNativeImageBuildArgs.addAll(nativeImageBuildArgs);
        }
        if (baseImageRun.contains("distroless") && !allNativeImageBuildArgs.contains(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG)) {
            allNativeImageBuildArgs.add(MOSTLY_STATIC_NATIVE_IMAGE_GRAALVM_FLAG);
        }

        Path argsFilePath = Paths.get(argsFile);
        if (Files.exists(argsFilePath)) {
            List<String> args = Files.readAllLines(argsFilePath);
            if (args.contains("-cp")) {
                int cpPosition = args.indexOf("-cp");
                args.remove(cpPosition);
                args.remove(cpPosition);
            }

            List<String> newArgs = args.stream()
                    .filter(arg -> !arg.startsWith("-H:Name"))
                    .filter(arg -> !arg.startsWith("-H:Class"))
                    .filter(arg -> !arg.startsWith("-H:Path"))
                    .filter(arg -> !arg.startsWith("-H:ConfigurationFileDirectories"))
                    .map(arg -> {
                        if (arg.startsWith("\\Q") && arg.endsWith("\\E")) {
                            int lastIndexOfSlash = arg.contains("/") ? arg.lastIndexOf("/") : arg.lastIndexOf("\\");
                            return "\\Q/home/app/libs" + arg.substring(lastIndexOfSlash);
                        } else {
                            return arg;
                        }
                    })
                    .toList();
            allNativeImageBuildArgs.addAll(newArgs);
        } else {
            throw new IOException("Unable to find args file: " + argsFilePath);
        }
        return allNativeImageBuildArgs;
    }
}
