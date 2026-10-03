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

import io.micronaut.core.annotation.Internal;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.jspecify.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * The JDK AOT cache training run of an application: the mode, and what ends the run. It is resolved from
 * {@code micronaut.docker.jdkAotCache.trainingMode}, the training paths and what the application's Micronaut version can
 * do.
 *
 * <p>Without a configured mode, the run is a {@code load} run if the Micronaut version has that mode, because it creates
 * no bean and so needs none of the services the beans use, which an image build does not have. Otherwise, it starts the
 * application, as every Micronaut version can.</p>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
@Internal
public final class TrainingRun {

    private static final String SCRIPT_LOAD = "load";
    private static final String SCRIPT_SWITCH = "switch";
    private static final String SCRIPT_SIGTERM = "sigterm";
    private static final String LOG_PREFIX = "JDK AOT cache: ";
    private static final String PATHS_OPTION = "micronaut.docker.jdkAotCache.trainingPaths";
    private static final String LOAD_OPTION = TrainingMode.OPTION + "=" + TrainingMode.LOAD.id();
    private static final String START_OPTION = TrainingMode.OPTION + "=" + TrainingMode.START.id();
    private static final String LOAD_SENDS_NO_REQUESTS = "a load training run does not start the application, so it "
        + "sends no requests.";

    private final TrainingMode mode;
    private final boolean usesSwitch;
    private final String description;
    private final @Nullable String warning;

    /**
     * @param mode how far the application goes
     * @param usesSwitch whether the Micronaut training-run switch ends the run. Otherwise, the training script warms the
     * application up and stops it with SIGTERM
     * @param description what the run does and why, for the build log
     * @param warning what the build has to change before the application moves to a Micronaut version with the load
     * mode, or {@code null} if nothing
     */
    TrainingRun(TrainingMode mode, boolean usesSwitch, String description, @Nullable String warning) {
        this.mode = mode;
        this.usesSwitch = usesSwitch;
        this.description = description;
        this.warning = warning;
    }

    /**
     * A training run with nothing to warn about.
     *
     * @param mode how far the application goes
     * @param usesSwitch whether the Micronaut training-run switch ends the run
     * @param description what the run does and why, for the build log
     */
    TrainingRun(TrainingMode mode, boolean usesSwitch, String description) {
        this(mode, usesSwitch, description, null);
    }

    /**
     * @param configuredMode the configured {@code micronaut.docker.jdkAotCache.trainingMode}, which may be {@code null}
     * or blank
     * @param trainingPaths the validated training paths
     * @param artifacts the resolved dependencies of the application
     * @return the training run
     * @throws MojoExecutionException if the mode is not a mode, if the application's Micronaut version cannot run it,
     * or if training paths are set for a run that does not start the application
     */
    public static TrainingRun resolve(String configuredMode, List<String> trainingPaths, Collection<Artifact> artifacts)
        throws MojoExecutionException {
        Optional<TrainingMode> configured = TrainingMode.parse(configuredMode);
        boolean warmUp = !trainingPaths.isEmpty();
        boolean loadAvailable = TrainingRunSwitch.hasLoadMode(artifacts);
        if (configured.orElse(null) == TrainingMode.LOAD) {
            return configuredLoad(warmUp, loadAvailable);
        }
        if (configured.isEmpty() && loadAvailable) {
            if (warmUp) {
                throw new MojoExecutionException(PATHS_OPTION + " is set, but the training mode is load, the default "
                    + "with this Micronaut version: " + LOAD_SENDS_NO_REQUESTS + " Set " + START_OPTION + " if the "
                    + "application can start in the image build, or remove the training paths.");
            }
            return new TrainingRun(TrainingMode.LOAD, true, "training mode load, the default: the application loads "
                + "its bean definitions and exits without starting, so the training needs none of the services that "
                + "its beans use. If the application can start in the image build, " + START_OPTION + " trains a more "
                + "complete cache");
        }
        boolean usesSwitch = TrainingRunSwitch.isAvailable(artifacts, warmUp);
        if (configured.isPresent()) {
            return new TrainingRun(TrainingMode.START, usesSwitch, "training mode start: the training run starts the "
                + "application");
        }
        // The build that sets training paths and no mode is the one that fails after a Micronaut upgrade
        String warning = warmUp ? PATHS_OPTION + " is set and " + TrainingMode.OPTION + " is not. This build will fail "
            + "once the application uses a Micronaut version with the load training mode: load becomes the default, "
            + "and " + LOAD_SENDS_NO_REQUESTS + " Set " + START_OPTION + " now to keep starting the application" : null;
        return new TrainingRun(TrainingMode.START, usesSwitch, "training mode start: the training run starts the "
            + "application, because its Micronaut version has no training mode that loads it without starting it ("
            + TrainingRunSwitch.MODE_PROPERTY + "). The services it needs at start-up must be reachable from the image "
            + "build. With a Micronaut version that has that mode, load becomes the default: set " + START_OPTION
            + " to keep starting the application", warning);
    }

    /**
     * An explicit {@code load} fails on a Micronaut version without the mode, instead of starting an application that
     * was declared unable to start. That is checked before the training paths, and its message covers them, so that
     * a build with both problems is told about both at once.
     */
    private static TrainingRun configuredLoad(boolean warmUp, boolean loadAvailable) throws MojoExecutionException {
        if (!loadAvailable) {
            throw new MojoExecutionException(LOAD_OPTION + " needs a Micronaut version with the load training mode ("
                + TrainingRunSwitch.MODE_PROPERTY + "), which the application's Micronaut version does not have. "
                + "Upgrade Micronaut, or remove the option: the training run then starts the application."
                + (warmUp ? " If you upgrade, remove " + PATHS_OPTION + " as well: " + LOAD_SENDS_NO_REQUESTS : ""));
        }
        if (warmUp) {
            throw new MojoExecutionException(PATHS_OPTION + " cannot be used with " + LOAD_OPTION + ": "
                + LOAD_SENDS_NO_REQUESTS + " Remove the training paths, or set " + START_OPTION + " if the application "
                + "can start in the image build.");
        }
        return new TrainingRun(TrainingMode.LOAD, true, "training mode load: the application loads its bean "
            + "definitions and exits without starting");
    }

    /**
     * @return how far the application goes
     */
    TrainingMode mode() {
        return mode;
    }

    /**
     * @return whether the Micronaut training-run switch ends the run. Otherwise, the training script warms the
     * application up and stops it with SIGTERM
     */
    public boolean usesSwitch() {
        return usesSwitch;
    }

    /**
     * @return what the run does and why, for the build log
     */
    String description() {
        return description;
    }

    /**
     * @return what the build has to change before the application moves to a Micronaut version with the load mode, or
     * {@code null} if nothing
     */
    @Nullable String warning() {
        return warning;
    }

    /**
     * Writes what the run does and why to the build log, and the warning, if there is one.
     *
     * @param log the Maven log
     */
    public void log(Log log) {
        log.info(LOG_PREFIX + description);
        if (warning != null) {
            log.warn(LOG_PREFIX + warning);
        }
    }

    /**
     * Writes what {@link #log(Log)} writes and, for a run that starts the application, how the training script of the
     * generated Dockerfile ends it.
     *
     * @param log the Maven log
     */
    public void logGeneratedDockerfileRun(Log log) {
        log(log);
        if (mode == TrainingMode.START) {
            log.info(LOG_PREFIX + (usesSwitch
                ? "the application warms itself up and exits (Micronaut training-run switch)"
                : "the training script warms the application up and stops it with SIGTERM"));
        }
    }

    /**
     * @param trainingPaths the validated training paths
     * @return the Java system properties of the training JVM: those of the Micronaut training-run switch when it ends
     * the run, none when the training script does
     */
    List<String> systemProperties(List<String> trainingPaths) {
        return usesSwitch ? TrainingRunSwitch.systemProperties(mode, trainingPaths) : List.of();
    }

    /**
     * @return the argument that tells the training script how to run the application: {@code load}, {@code switch} or
     * {@code sigterm}
     */
    public String scriptArgument() {
        if (mode == TrainingMode.LOAD) {
            return SCRIPT_LOAD;
        }
        return usesSwitch ? SCRIPT_SWITCH : SCRIPT_SIGTERM;
    }
}
