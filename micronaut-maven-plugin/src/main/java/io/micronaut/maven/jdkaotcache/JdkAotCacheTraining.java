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
package io.micronaut.maven.jdkaotcache;

import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.InternetProtocol;
import io.micronaut.core.annotation.Internal;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Trains a JDK AOT cache in a container of the training image that the {@code docker} goal built with Jib. The
 * container runs the image's own entrypoint, with {@code -XX:AOTCacheOutput} and {@code -XX:-UsePerfData} in
 * {@code JDK_JAVA_OPTIONS}. What the application does in it is the {@link TrainingRun}:
 * <ul>
 *     <li>In a {@link TrainingMode#LOAD} run, it loads its bean definitions and exits without starting.</li>
 *     <li>In a {@link TrainingMode#START} run, if its Micronaut version has the training-run switch, it starts, warms
 *     itself up and exits. It fails on a request without a response or with a status of 500 or more, and only warns
 *     about a status from 400 to 499, so its output is copied to the build log when there are training paths.</li>
 *     <li>Otherwise, it starts, the warm-up script runs in the container with {@code docker exec} and the application
 *     is stopped with SIGTERM.</li>
 * </ul>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public final class JdkAotCacheTraining {

    /**
     * The minimum Java version that can train a JDK AOT cache in one step ({@code -XX:AOTCacheOutput}).
     */
    public static final int MINIMUM_JAVA_VERSION = 25;

    /**
     * The warm-up and training script, shipped with the plugin.
     */
    public static final String SCRIPT_RESOURCE = "/jdkAotCacheScripts/training.sh";

    static final String CONTAINER_CACHE_FILE = "/tmp/app.aot";
    /**
     * Keeps the training JVMs from writing performance data files ({@code /tmp/hsperfdata_<user>/<pid>}). The JVM that
     * {@code -XX:AOTCacheOutput} starts to write the cache reads {@code JDK_JAVA_OPTIONS} too, and leaves its file
     * behind. The training script passes the same option, because the layer of the {@code RUN} instruction that trains
     * the cache in the generated Dockerfile would keep that file.
     */
    static final String NO_PERF_DATA = "-XX:-UsePerfData";
    static final String JDK_JAVA_OPTIONS = "JDK_JAVA_OPTIONS";
    static final String JDK_AOT_VM_OPTIONS = "JDK_AOT_VM_OPTIONS";
    static final String COMPATIBLE_OOP_COMPRESSION_FLAG = "AOTCompatibleOopCompression";
    private static final int SIGTERM_EXIT_CODE = 143;
    private static final int COMMAND_NOT_EXECUTABLE = 126;
    private static final int COMMAND_NOT_FOUND = 127;
    private static final Pattern JAVA_VERSION = Pattern.compile(" version \"(\\d+)(?:\\.(\\d+))?");
    private static final Pattern COMPATIBLE_OOP_COMPRESSION = Pattern.compile("(?m)^\\s*bool\\s+" + COMPATIBLE_OOP_COMPRESSION_FLAG + "\\s");

    private final DockerService dockerService;
    private final Log log;
    private final List<String> trainingPaths;
    private final int timeoutSeconds;
    private final String networkMode;

    /**
     * @param dockerService the Docker service
     * @param log the Maven log
     * @param trainingPaths the paths of the warm-up GET requests
     * @param timeoutSeconds the timeout of each step of the training run
     * @param networkMode the network of the training container, if any
     */
    public JdkAotCacheTraining(DockerService dockerService, Log log, List<String> trainingPaths, int timeoutSeconds,
                               String networkMode) {
        this.dockerService = dockerService;
        this.log = log;
        this.trainingPaths = trainingPaths;
        this.timeoutSeconds = timeoutSeconds;
        this.networkMode = networkMode;
    }

    /**
     * Runs the training image once and copies the cache it writes out of the container.
     *
     * @param imageId the training image
     * @param run the training run
     * @param cacheFile where to write the cache
     * @throws MojoExecutionException if the training fails
     */
    public void train(String imageId, TrainingRun run, Path cacheFile) throws MojoExecutionException {
        InspectImageResponse image = dockerService.inspectImage(imageId);
        String[] imageEnvironment = image.getConfig() == null ? null : image.getConfig().getEnv();
        JavaRuntime java = probeJava(imageId);
        Integer port = run.usesSwitch() ? null : httpPort(image);
        Map<String, String> environment = trainingEnvironment(imageEnvironment, java, run, trainingPaths);
        environment.forEach((name, value) -> log.info("JDK AOT cache: training with " + name + "=" + value));

        String containerId = dockerService.createContainer(imageId, networkMode, false, environment);
        try {
            if (run.mode() == TrainingMode.LOAD) {
                log.info("JDK AOT cache: the application loads its bean definitions and exits without starting "
                    + "(Micronaut training mode load)");
                dockerService.startAndWait(containerId, imageId, timeoutSeconds);
                // The summary of the run and the bean definitions it skipped say what the cache was trained on
                dockerService.logContainerOutput(containerId);
            } else if (run.usesSwitch()) {
                log.info("JDK AOT cache: the application warms itself up and exits (Micronaut training-run switch)");
                dockerService.startAndWait(containerId, imageId, timeoutSeconds);
                if (!trainingPaths.isEmpty()) {
                    // Micronaut only warns about a request answered with 400 to 499, in the output of the application
                    dockerService.logContainerOutput(containerId);
                }
            } else {
                log.info("JDK AOT cache: warming the application up with docker exec, then stopping it with SIGTERM "
                    + "(the application's Micronaut version has no training-run switch)");
                dockerService.startContainer(containerId);
                // httpPort(image) set the port, because this run does not use the switch
                warmUp(containerId, Objects.requireNonNull(port, "port"));
                log.info("JDK AOT cache: stopping the application with SIGTERM");
                dockerService.signalContainer(containerId, "SIGTERM");
                int exitCode = dockerService.awaitExit(containerId, timeoutSeconds);
                if (exitCode != SIGTERM_EXIT_CODE && exitCode != 0) {
                    dockerService.logContainerOutput(containerId);
                    throw new MojoExecutionException("JDK AOT cache training failed: the application exited with status "
                        + exitCode + " after SIGTERM");
                }
            }
            copyCache(containerId, cacheFile);
        } catch (IOException e) {
            throw new MojoExecutionException("JDK AOT cache training failed: " + e.getMessage(), e);
        } finally {
            dockerService.removeContainer(containerId);
        }
    }

    /**
     * @param paths the configured training paths
     * @return the paths, trimmed
     * @throws MojoExecutionException if a path is not an absolute request path
     */
    public static List<String> validateTrainingPaths(List<String> paths) throws MojoExecutionException {
        if (paths == null) {
            return List.of();
        }
        var result = new ArrayList<String>(paths.size());
        for (String path : paths) {
            String trimmed = path == null ? "" : path.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            if (!trimmed.startsWith("/") || trimmed.chars().anyMatch(c -> Character.isWhitespace(c) || Character.isISOControl(c))) {
                throw new MojoExecutionException("Invalid micronaut.docker.jdkAotCache.trainingPaths entry '" + trimmed
                    + "': it must start with / and contain no spaces or control characters");
            }
            result.add(trimmed);
        }
        return List.copyOf(result);
    }

    /**
     * @return the warm-up and training script
     * @throws IOException if the script cannot be read
     */
    public static String readScript() throws IOException {
        try (InputStream in = JdkAotCacheTraining.class.getResourceAsStream(SCRIPT_RESOURCE)) {
            if (in == null) {
                throw new IOException("Could not find " + SCRIPT_RESOURCE);
            }
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    static Map<String, String> trainingEnvironment(String[] imageEnvironment, JavaRuntime java, TrainingRun run,
                                                  List<String> trainingPaths) {
        var environment = new LinkedHashMap<String, String>();
        var options = new ArrayList<String>();
        imageVariable(imageEnvironment, JDK_JAVA_OPTIONS).ifPresent(options::add);
        options.add("-XX:AOTCacheOutput=" + CONTAINER_CACHE_FILE);
        options.add(NO_PERF_DATA);
        options.addAll(run.systemProperties(trainingPaths));
        environment.put(JDK_JAVA_OPTIONS, String.join(" ", options));
        if (java.compatibleOopCompression()) {
            // JDK 27 and later: lets the cache work with another compressed oops encoding than the training one
            var aotOptions = new ArrayList<String>();
            imageVariable(imageEnvironment, JDK_AOT_VM_OPTIONS).ifPresent(aotOptions::add);
            aotOptions.add("-XX:+UnlockDiagnosticVMOptions");
            aotOptions.add("-XX:+" + COMPATIBLE_OOP_COMPRESSION_FLAG);
            environment.put(JDK_AOT_VM_OPTIONS, String.join(" ", aotOptions));
        }
        return environment;
    }

    static Optional<Integer> firstTcpPort(ExposedPort[] exposedPorts) {
        if (exposedPorts == null) {
            return Optional.empty();
        }
        return Arrays.stream(exposedPorts)
            .filter(Objects::nonNull)
            .filter(port -> port.getProtocol() == null || port.getProtocol() == InternetProtocol.TCP)
            .map(ExposedPort::getPort)
            .findFirst();
    }

    private static Optional<String> imageVariable(String[] imageEnvironment, String name) {
        if (imageEnvironment == null) {
            return Optional.empty();
        }
        String prefix = name + "=";
        return Arrays.stream(imageEnvironment)
            .filter(variable -> variable != null && variable.startsWith(prefix))
            .map(variable -> variable.substring(prefix.length()))
            .filter(value -> !value.isBlank())
            .reduce((first, second) -> second);
    }

    private JavaRuntime probeJava(String imageId) throws MojoExecutionException {
        DockerService.ContainerOutput output;
        try {
            output = dockerService.runAndCaptureOutput(imageId, timeoutSeconds,
                List.of("java", "-XX:+UnlockDiagnosticVMOptions", "-XX:+PrintFlagsFinal", "-version"));
        } catch (IOException e) {
            throw new MojoExecutionException("JDK AOT cache training failed: could not run java -version in the training image: "
                + e.getMessage(), e);
        }
        Optional<JavaRuntime> java = JavaRuntime.parse(output.output());
        if (output.exitCode() != 0 || java.isEmpty()) {
            throw new MojoExecutionException("JDK AOT cache training failed: java -version exited with status "
                + output.exitCode() + " in the training image: " + lastLines(output.output()));
        }
        if (java.get().majorVersion() < MINIMUM_JAVA_VERSION) {
            throw new MojoExecutionException("micronaut.docker.jdkAotCache needs Java " + MINIMUM_JAVA_VERSION
                + " or later in the image, but the base image runs Java " + java.get().majorVersion()
                + ". Use a base image with Java " + MINIMUM_JAVA_VERSION + " or later.");
        }
        log.info("JDK AOT cache: the training image runs Java " + java.get().majorVersion());
        return java.get();
    }

    private int httpPort(InspectImageResponse image) throws MojoExecutionException {
        return firstTcpPort(image.getConfig() == null ? null : image.getConfig().getExposedPorts())
            .orElseThrow(() -> new MojoExecutionException("micronaut.docker.jdkAotCache needs the HTTP port of the "
                + "application to warm it up, but the image exposes no port because the server port is resolved "
                + "dynamically. Expose the port with the Jib container.ports configuration."));
    }

    private void warmUp(String containerId, int port) throws IOException, MojoExecutionException {
        var command = new ArrayList<>(List.of("bash", "-c", readScript(), "training.sh", "warm-up",
            String.valueOf(port), String.valueOf(timeoutSeconds)));
        command.addAll(trainingPaths);
        var output = new ArrayList<String>();
        int exitCode = dockerService.execInContainer(containerId, (trainingPaths.size() + 1) * timeoutSeconds, line -> {
            output.add(line);
            log.info(line);
        }, command.toArray(String[]::new));
        if (exitCode != 0) {
            dockerService.logContainerOutput(containerId);
            if (exitCode == COMMAND_NOT_EXECUTABLE || exitCode == COMMAND_NOT_FOUND) {
                throw new MojoExecutionException("JDK AOT cache training failed: the warm-up needs bash in the image, "
                    + "which it could not run (status " + exitCode + "). Use a base image with bash, or a Micronaut "
                    + "version with the training-run switch.");
            }
            throw new MojoExecutionException("JDK AOT cache training failed: "
                + (output.isEmpty() ? "the warm-up exited with status " + exitCode : output.get(output.size() - 1)));
        }
    }

    private void copyCache(String containerId, Path cacheFile) throws IOException, MojoExecutionException {
        try {
            dockerService.copyFileFromContainer(containerId, CONTAINER_CACHE_FILE, cacheFile);
        } catch (IOException e) {
            dockerService.logContainerOutput(containerId);
            throw new MojoExecutionException("JDK AOT cache training failed: the training run did not write the cache ("
                + e.getMessage() + ")", e);
        }
        long size = Files.size(cacheFile);
        if (size == 0) {
            dockerService.logContainerOutput(containerId);
            throw new MojoExecutionException("JDK AOT cache training failed: the training run wrote an empty cache");
        }
        // Decimal megabytes, as Docker gives the size of the image layer that holds the cache
        log.info("JDK AOT cache: trained " + cacheFile + " (" + String.format(Locale.ROOT, "%.1f MB", size / 1e6)
            + ")");
    }

    private static String lastLines(String output) {
        List<String> lines = output.lines().filter(line -> !line.isBlank()).toList();
        return String.join(System.lineSeparator(), lines.subList(Math.max(0, lines.size() - 3), lines.size()));
    }

    /**
     * The Java runtime of the training image.
     *
     * @param majorVersion the Java feature version
     * @param compatibleOopCompression whether the JVM has the {@code AOTCompatibleOopCompression} diagnostic flag
     */
    record JavaRuntime(int majorVersion, boolean compatibleOopCompression) {

        /**
         * @param output the output of {@code java -XX:+UnlockDiagnosticVMOptions -XX:+PrintFlagsFinal -version}
         * @return the Java runtime, if the output has a version line with a valid version number
         */
        static Optional<JavaRuntime> parse(String output) {
            Matcher matcher = JAVA_VERSION.matcher(output);
            if (!matcher.find()) {
                return Optional.empty();
            }
            try {
                int major = Integer.parseInt(matcher.group(1));
                if (major == 1 && matcher.group(2) != null) {
                    major = Integer.parseInt(matcher.group(2));
                }
                return Optional.of(new JavaRuntime(major, COMPATIBLE_OOP_COMPRESSION.matcher(output).find()));
            } catch (NumberFormatException _) {
                // a version number too large for an int
                return Optional.empty();
            }
        }
    }
}
