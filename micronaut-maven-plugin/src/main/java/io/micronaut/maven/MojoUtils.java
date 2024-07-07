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
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.Os;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
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
        System.out.println("------ HERE ---------"+ argsFile);
        Path argsFilePath = Paths.get(argsFile);
        if (Files.exists(argsFilePath)) {
            List<String> args;
            try {
                args = Files.readAllLines(argsFilePath);
                args = ignorePathWhiteSpaces(args);
                System.out.println("resulted args:\n/I" + args);
                /*if(argsFilePath.toString().contains("docker-native")) {
                    System.out.println("TRUE");
                    String[] test = new String[]{
                            "-Ob",
                            "--no-fallback",
                            "-o",
                            "\"\"C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\package-docker-native\"\"",
                            "\"\"-H:ConfnFileDirectories=C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-common\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-handler\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-buffer\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-codec-http2\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-codec-http\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\io.netty\\n" + //
                                    "etty-transport\\4.1.80.Final,C:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\it\\package-docker-native\\target\\graalvm-reachability-metadata\\2f4c00fdf0445087b50fa34cf9fa6555ab490d8d\\ch.qos.logback\\logback-classic\\1.4.9\"\"" ,
                            "--no-fallback",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\n" + //
                                    "etty\\n" + //
                                    "etty-codec-http2\\4.1.108.Final\\n" + //
                                    "etty-codec-http2-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\n" + //
                                    "etty\\n" + //
                                    "etty-handler\\4.1.108.Final\\n" + //
                                    "etty-handler-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\n" + //
                                    "etty\\n" + //
                                    "etty-codec-http\\4.1.108.Final\\n" + //
                                    "etty-codec-http-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\n" + //
                                    "etty\\n" + //
                                    "etty-common\\4.1.108.Final\\n" + //
                                    "etty-common-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\n" + //
                                    "etty\\n" + //
                                    "etty-buffer\\4.1.108.Final\\n" + //
                                    "etty-buffer-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "--exclude-config",
                            "\\QC:\\Users\\Lahoucine EL ADDALI\\Desktop\\projects-p\\micronaut-maven-plugin\\target\\local-repo\\io\\netty\\netty-transport\\4.1.108.Final\\netty-transport-4.1.108.Final.jar\\E",
                            "^/META-INF/native-image/",
                            "-H:ConfigurationFileDirectories=/home/app/graalvm-reachability-metadata/generateResourceConfig,/generateTestResourceConfig"
                    };
                    args= Arrays.asList(test);
                    System.out.println("ARGS:\n" + args);
                }*/
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
                        arg= arg.replace("\"", "");
                        if (arg.startsWith("@")) {
                            String fileName = arg.substring(1);
                            return parseNativeImageArgsFile(fileName);
                        } else if (arg.startsWith("\\\\Q") && arg.endsWith("\\\\E")) {
                            // start the search at length - 3 to skip \Q or \E at the end
                            int lastIndexOfSlash = arg.lastIndexOf(File.separator , arg.length() - 4);
                            System.out.println("HERE1" + arg.substring(lastIndexOfSlash ));
                            return Stream.of("\\Q/home/app/libs/" + arg.substring(lastIndexOfSlash + 1,arg.length()-3) +"\\E");
                        } else if (arg.startsWith("-H:ConfigurationFileDirectories")) {
                            System.out.println("HERE2 CONFIG");
                            return Stream.of(parseConfigurationFilesDirectoriesArg(arg));
                        }else {
                            return Stream.of(arg);
                        }
                    });
        } else {
            throw new RuntimeException("Unable to find args file: " + argsFilePath);
        }
    }

    private static List<String> ignorePathWhiteSpaces(List<String> args) {
        int i=0;
        List<String> result = new ArrayList<>();
        while(i < args.size()) {

            if(args.get(i).startsWith("\\\\Q") &&
                    !args.get(i).endsWith("\\\\E")) {
                System.out.println("HERE");
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
                    System.out.println("CCCC");
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
            System.out.println("HOUCINE");
            return Stream.of(directories)
                    .map(FilenameUtils::separatorsToUnix)
                    .map(directory -> {
                        String[] splitDirectory = directory.replace("//","/").split(separator);
                        System.out.println(Arrays.toString(splitDirectory));
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
