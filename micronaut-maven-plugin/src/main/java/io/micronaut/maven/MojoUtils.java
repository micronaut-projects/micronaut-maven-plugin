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

import org.apache.commons.io.FilenameUtils;
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
                // for testing
                // ignore whitespaces in the generated args
                // TODO: fix the write-args-file from the maven build-tool plugin
                args = ignorePathWhiteSpaces(args);
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
                        arg= arg.replace("\"", ""); // remove the " quotation from the arg
                        String[] patterns = new String[]{"\\Q","\\E"};
                        boolean isWindows = false;
                        if(File.separator.equals("\\")) {
                            patterns = new String[]{"\\\\Q","\\\\E"};
                            isWindows=true;
                        }
                        if (arg.startsWith("@")) {
                            String fileName = arg.substring(1);
                            return parseNativeImageArgsFile(fileName);
                        } else if (arg.startsWith(patterns[0]) && arg.endsWith(patterns[1])) {
                            // start the search at length - 3 to skip \Q or \E at the end
                            int skip = isWindows ? 4: 3;
                            int lastIndexOfSlash = arg.lastIndexOf(File.separator , arg.length() - skip);
                            if(isWindows) {
                                return Stream.of("\\Q/home/app/libs/" + arg.substring(lastIndexOfSlash + 1,arg.length()-3) +"\\E");
                            }
                            return Stream.of("\\Q/home/app/libs/" + arg.substring(lastIndexOfSlash + 1));
                        } else if (arg.startsWith("-H:ConfigurationFileDirectories")) {
                            return Stream.of(parseConfigurationFilesDirectoriesArg(arg));
                        }else {
                            return Stream.of(arg);
                        }
                    });
        } else {
            throw new RuntimeException("Unable to find args file: " + argsFilePath);
        }
    }

    // remove lines breaks introduced by write-args-file
    // with maven build-tools plugin
    private static List<String> ignorePathWhiteSpaces(List<String> args) {
        int i=0;
        List<String> result = new ArrayList<>();
        while(i < args.size()) {
            if(args.get(i).startsWith("\\\\Q") &&
                    !args.get(i).endsWith("\\\\E")) {
                StringBuilder line = new StringBuilder(args.get(i));
                i++;
                while(!args.get(i).endsWith("\\E")) {
                    line.append(" ").append(args.get(i));
                    i++;
                }
                line.append(" ").append(args.get(i));
                result.add(line.toString());
                i++;
            }else if(args.get(i).startsWith("-H:ConfigurationFileDirectories")) {
                StringBuilder line = new StringBuilder(args.get(i));
                i++;
                while( i< args.size() && args.get(i).toLowerCase().charAt(0) <='z'
                        && args.get(i).toLowerCase().charAt(0) >='a') {
                    line.append(" ").append(args.get(i));
                    i++;
                }
                result.add(line.toString());
                i++;
            }
            else{
                result.add(args.get(i));
                i++;
            }
        }
        return result;
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
                        String[] splitDirectory = directory.replaceAll("//","/").split(separator);
                        String last4Directories = splitDirectory[splitDirectory.length - 4] + separator +
                                splitDirectory[splitDirectory.length - 3] + separator +
                                splitDirectory[splitDirectory.length - 2] + separator +
                                splitDirectory[splitDirectory.length - 1];
                        return "/home/app/graalvm-reachability-metadata/" +last4Directories;
                    })
                    .collect(Collectors.joining(","))
                    .transform(s -> "-H:ConfigurationFileDirectories=" + s);
        }
    }
}
