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
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;

import java.util.List;

/**
 * Compatibility facade for utility methods now shared from {@code micronaut-maven-core}.
 */
public final class MojoUtils {

    public static final String THIS_PLUGIN = io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;

    private MojoUtils() {
    }

    public static String findJavaExecutable(ToolchainManager toolchainManager, MavenSession mavenSession) {
        return io.micronaut.maven.core.MojoUtils.findJavaExecutable(toolchainManager, mavenSession);
    }

    public static List<String> computeNativeImageArgs(List<String> nativeImageBuildArgs, String baseImageRun, String argsFile) {
        return io.micronaut.maven.core.MojoUtils.computeNativeImageArgs(nativeImageBuildArgs, baseImageRun, argsFile);
    }

    static String parseConfigurationFilesDirectoriesArg(String arg) {
        return io.micronaut.maven.core.MojoUtils.parseConfigurationFilesDirectoriesArg(arg);
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
