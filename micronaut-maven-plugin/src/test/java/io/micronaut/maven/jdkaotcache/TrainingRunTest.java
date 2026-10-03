package io.micronaut.maven.jdkaotcache;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

class TrainingRunTest {

    private static final List<String> NO_PATHS = List.of();
    private static final List<String> PATHS = List.of("/hello");

    @Test
    void loadIsTheDefaultWhenTheMicronautVersionHasTheMode(@TempDir Path tempDir) throws Exception {
        TrainingRun run = TrainingRun.resolve(null, NO_PATHS, MicronautJars.withLoadMode(tempDir));

        assertEquals(TrainingMode.LOAD, run.mode());
        assertTrue(run.usesSwitch());
        assertEquals("load", run.scriptArgument());
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true", "-Dmicronaut.application.training.mode=load"),
            run.systemProperties(NO_PATHS));
        assertEquals("training mode load, the default: the application loads its bean definitions and exits without "
            + "starting, so the training needs none of the services that its beans use. If the application can start "
            + "in the image build, micronaut.docker.jdkAotCache.trainingMode=start trains a more complete cache",
            run.description());
        assertNull(run.warning());
    }

    @Test
    void aBlankModeIsTheDefault(@TempDir Path tempDir) throws Exception {
        assertEquals(TrainingMode.LOAD, TrainingRun.resolve(" ", NO_PATHS, MicronautJars.withLoadMode(tempDir)).mode());
    }

    @Test
    void loadCanBeConfigured(@TempDir Path tempDir) throws Exception {
        TrainingRun run = TrainingRun.resolve("LOAD", NO_PATHS, MicronautJars.withLoadMode(tempDir));

        assertEquals(TrainingMode.LOAD, run.mode());
        assertTrue(run.usesSwitch());
        assertEquals("training mode load: the application loads its bean definitions and exits without starting", run.description());
    }

    @Test
    void startIsAnOptInWhenTheMicronautVersionHasTheMode(@TempDir Path tempDir) throws Exception {
        TrainingRun run = TrainingRun.resolve("start", PATHS, MicronautJars.withLoadMode(tempDir));

        assertEquals(TrainingMode.START, run.mode());
        assertTrue(run.usesSwitch());
        assertEquals("switch", run.scriptArgument());
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true", "-Dmicronaut.application.training.mode=start",
            "-Dmicronaut.application.training.warmup.paths[0]=/hello"), run.systemProperties(PATHS));
        assertEquals("training mode start: the training run starts the application", run.description());
    }

    @Test
    void startWithoutPathsIsAnOptInToo(@TempDir Path tempDir) throws Exception {
        TrainingRun run = TrainingRun.resolve("start", NO_PATHS, MicronautJars.withLoadMode(tempDir));

        assertEquals(TrainingMode.START, run.mode());
        assertEquals(List.of("-Dmicronaut.application.training.enabled=true", "-Dmicronaut.application.training.mode=start"),
            run.systemProperties(NO_PATHS));
    }

    @Test
    void aVersionWithTheSwitchAndNoModeStartsTheApplicationAndSaysSo(@TempDir Path tempDir) throws Exception {
        for (List<String> paths : List.of(NO_PATHS, PATHS)) {
            TrainingRun run = TrainingRun.resolve(null, paths, MicronautJars.withSwitch(tempDir));

            assertEquals(TrainingMode.START, run.mode());
            assertTrue(run.usesSwitch());
            assertEquals("switch", run.scriptArgument());
            assertDefaultStartDescription(run);
            assertUpgradeWarning(run, paths);
        }
    }

    @Test
    void aVersionWithoutTheSwitchStartsTheApplicationWithTheScriptAndSaysSo(@TempDir Path tempDir) throws Exception {
        for (List<String> paths : List.of(NO_PATHS, PATHS)) {
            TrainingRun run = TrainingRun.resolve(null, paths, MicronautJars.withoutSwitch(tempDir));

            assertEquals(TrainingMode.START, run.mode());
            assertFalse(run.usesSwitch());
            assertEquals("sigterm", run.scriptArgument());
            assertEquals(List.of(), run.systemProperties(paths));
            assertDefaultStartDescription(run);
            assertUpgradeWarning(run, paths);
        }
    }

    @Test
    void anExplicitStartOnAVersionWithoutTheModeDoesNotExplainTheDefault(@TempDir Path tempDir) throws Exception {
        TrainingRun withSwitch = TrainingRun.resolve("start", PATHS, MicronautJars.withSwitch(tempDir));
        TrainingRun withoutSwitch = TrainingRun.resolve("start", PATHS, MicronautJars.withoutSwitch(tempDir));

        for (TrainingRun run : List.of(withSwitch, withoutSwitch)) {
            assertEquals(TrainingMode.START, run.mode());
            assertEquals("training mode start: the training run starts the application", run.description());
            assertNull(run.warning());
        }
        assertTrue(withSwitch.usesSwitch());
        assertFalse(withoutSwitch.usesSwitch());
    }

    @Test
    void theWarmUpNeedsMicronautHttpServerAlsoWhenTheVersionHasTheMode(@TempDir Path tempDir) throws Exception {
        var artifacts = List.of(MicronautJars.contextWithLoadMode(tempDir));

        assertTrue(TrainingRun.resolve("start", NO_PATHS, artifacts).usesSwitch());
        assertFalse(TrainingRun.resolve("start", PATHS, artifacts).usesSwitch());
    }

    @Test
    void trainingPathsFailWithTheDefaultLoadMode(@TempDir Path tempDir) throws IOException {
        var artifacts = MicronautJars.withLoadMode(tempDir);

        var e = assertThrows(MojoExecutionException.class, () -> TrainingRun.resolve(null, PATHS, artifacts));

        assertEquals("micronaut.docker.jdkAotCache.trainingPaths is set, but the training mode is load, the default with "
            + "this Micronaut version: a load training run does not start the application, so it sends no requests. Set "
            + "micronaut.docker.jdkAotCache.trainingMode=start if the application can start in the image build, or "
            + "remove the training paths.", e.getMessage());
    }

    @Test
    void trainingPathsFailWithAConfiguredLoadMode(@TempDir Path tempDir) throws IOException {
        var artifacts = MicronautJars.withLoadMode(tempDir);

        var e = assertThrows(MojoExecutionException.class, () -> TrainingRun.resolve("load", PATHS, artifacts));

        assertEquals("micronaut.docker.jdkAotCache.trainingPaths cannot be used with "
            + "micronaut.docker.jdkAotCache.trainingMode=load: a load training run does not start the application, so "
            + "it sends no requests. Remove the training paths, or set micronaut.docker.jdkAotCache.trainingMode=start "
            + "if the application can start in the image build.", e.getMessage());
    }

    @Test
    void aConfiguredLoadModeWithTrainingPathsOnAVersionWithoutTheModeNamesBothProblemsAtOnce(@TempDir Path tempDir) throws IOException {
        for (var artifacts : List.of(MicronautJars.withSwitch(tempDir), MicronautJars.withoutSwitch(tempDir))) {
            var e = assertThrows(MojoExecutionException.class, () -> TrainingRun.resolve("load", PATHS, artifacts));

            // Removing the paths alone would not fix the build, so the missing mode comes first
            assertEquals("micronaut.docker.jdkAotCache.trainingMode=load needs a Micronaut version with the load training "
                + "mode (micronaut.application.training.mode), which the application's Micronaut version does not have. "
                + "Upgrade Micronaut, or remove the option: the training run then starts the application. If you "
                + "upgrade, remove micronaut.docker.jdkAotCache.trainingPaths as well: a load training run does not "
                + "start the application, so it sends no requests.", e.getMessage());
        }
    }

    @Test
    void aConfiguredLoadModeFailsOnAVersionWithoutTheMode(@TempDir Path tempDir) throws IOException {
        for (var artifacts : List.of(MicronautJars.withSwitch(tempDir), MicronautJars.withoutSwitch(tempDir))) {
            var e = assertThrows(MojoExecutionException.class, () -> TrainingRun.resolve("load", NO_PATHS, artifacts));

            assertEquals("micronaut.docker.jdkAotCache.trainingMode=load needs a Micronaut version with the load training "
                + "mode (micronaut.application.training.mode), which the application's Micronaut version does not have. "
                + "Upgrade Micronaut, or remove the option: the training run then starts the application.", e.getMessage());
        }
    }

    @Test
    void anUnknownModeFails(@TempDir Path tempDir) throws IOException {
        var artifacts = MicronautJars.withLoadMode(tempDir);

        var e = assertThrows(MojoExecutionException.class, () -> TrainingRun.resolve("warm", NO_PATHS, artifacts));

        assertEquals("Invalid micronaut.docker.jdkAotCache.trainingMode 'warm': it must be load or start", e.getMessage());
    }

    @Test
    void logsTheDescriptionAndNothingElseWithoutAWarning() {
        Log log = mock(Log.class);

        new TrainingRun(TrainingMode.LOAD, true, "training mode load").log(log);

        verify(log).info("JDK AOT cache: training mode load");
        verifyNoMoreInteractions(log);
    }

    @Test
    void logsTheWarningAtWarnLevel() {
        Log log = mock(Log.class);

        new TrainingRun(TrainingMode.START, false, "training mode start", "set the mode").log(log);

        verify(log).info("JDK AOT cache: training mode start");
        verify(log).warn("JDK AOT cache: set the mode");
        verifyNoMoreInteractions(log);
    }

    @Test
    void logsHowTheGeneratedDockerfileEndsARunThatStartsTheApplication() {
        Log switchLog = mock(Log.class);
        Log scriptLog = mock(Log.class);
        Log loadLog = mock(Log.class);

        new TrainingRun(TrainingMode.START, true, "training mode start", "set the mode").logGeneratedDockerfileRun(switchLog);
        new TrainingRun(TrainingMode.START, false, "training mode start").logGeneratedDockerfileRun(scriptLog);
        new TrainingRun(TrainingMode.LOAD, true, "training mode load").logGeneratedDockerfileRun(loadLog);

        var switchOrder = inOrder(switchLog);
        switchOrder.verify(switchLog).info("JDK AOT cache: training mode start");
        switchOrder.verify(switchLog).warn("JDK AOT cache: set the mode");
        switchOrder.verify(switchLog).info("JDK AOT cache: the application warms itself up and exits (Micronaut "
            + "training-run switch)");
        verifyNoMoreInteractions(switchLog);
        verify(scriptLog).info("JDK AOT cache: training mode start");
        verify(scriptLog).info("JDK AOT cache: the training script warms the application up and stops it with SIGTERM");
        verifyNoMoreInteractions(scriptLog);
        // The description of a load run already says that the application exits without starting
        verify(loadLog).info("JDK AOT cache: training mode load");
        verifyNoMoreInteractions(loadLog);
    }

    /**
     * A build that sets training paths and no mode is the one that fails once the Micronaut version has the load mode.
     */
    private static void assertUpgradeWarning(TrainingRun run, List<String> paths) {
        if (paths.isEmpty()) {
            assertNull(run.warning());
        } else {
            assertEquals("micronaut.docker.jdkAotCache.trainingPaths is set and micronaut.docker.jdkAotCache.trainingMode "
                + "is not. This build will fail once the application uses a Micronaut version with the load training "
                + "mode: load becomes the default, and a load training run does not start the application, so it sends "
                + "no requests. Set micronaut.docker.jdkAotCache.trainingMode=start now to keep starting the application",
                run.warning());
        }
    }

    private static void assertDefaultStartDescription(TrainingRun run) {
        assertEquals("training mode start: the training run starts the application, because its Micronaut version has no "
            + "training mode that loads it without starting it (micronaut.application.training.mode). The services it "
            + "needs at start-up must be reachable from the image build. With a Micronaut version that has that mode, "
            + "load becomes the default: set micronaut.docker.jdkAotCache.trainingMode=start to keep starting the "
            + "application", run.description());
    }
}
