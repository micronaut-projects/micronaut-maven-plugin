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
package io.micronaut.maven.core;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.Os;

import java.io.File;

/**
 * Shared utility methods for Micronaut Maven plugin modules.
 */
public final class MojoUtils {

    public static final String THIS_PLUGIN = "io.micronaut.maven:micronaut-maven-plugin";
    private static final String JAVA = "java";

    private MojoUtils() {
    }

    public static String findJavaExecutable(ToolchainManager toolchainManager, MavenSession mavenSession) {
        String executable;
        Toolchain toolchain = toolchainManager.getToolchainFromBuildContext("jdk", mavenSession);
        if (toolchain != null) {
            executable = toolchain.findTool(JAVA);
        } else {
            executable = null;
        }

        // Fallback to default Java executable if toolchain is not configured or doesn't provide a valid tool.
        if (executable == null) {
            var javaBinariesDir = new File(new File(System.getProperty("java.home")), "bin");
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

    public static boolean hasMicronautMavenPlugin(MavenProject project) {
        String[] parts = THIS_PLUGIN.split(":");
        String groupId = parts[0];
        String artifactId = parts[1];
        return project.getBuildPlugins().stream()
            .anyMatch(p -> p.getGroupId().equals(groupId) && p.getArtifactId().equals(artifactId));
    }
}
