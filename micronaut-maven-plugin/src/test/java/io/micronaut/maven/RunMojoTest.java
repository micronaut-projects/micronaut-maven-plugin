package io.micronaut.maven;

import io.micronaut.maven.aot.AbstractAotAnalysisMojo;
import io.micronaut.maven.core.MojoUtils;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.toolchain.ToolchainManager;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static io.micronaut.maven.core.MojoUtils.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class RunMojoTest {

    @TempDir
    Path tempDir;

    private RunMojo recordingMojo;

    @Test
    void runAotPassesExplicitEnabledConfiguration() throws Exception {
        MavenSession mavenSession = mock(MavenSession.class);
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        ExecutorService executorService = mock(ExecutorService.class);
        MavenProject runnableProject = new MavenProject();
        RunMojo mojo = new RunMojo(
            mavenSession,
            mock(BuildPluginManager.class),
            mock(ProjectBuilder.class),
            toolchainManager,
            mock(CompilerService.class),
            executorService,
            mock(DependencyResolutionService.class)
        );
        setField(mojo, "aotEnabled", true);
        setField(mojo, "runnableProject", runnableProject);

        invokeRunAotIfNeeded(mojo);

        ArgumentCaptor<Xpp3Dom> configurationCaptor = ArgumentCaptor.forClass(Xpp3Dom.class);
        verify(executorService).executeGoal(
            eq(runnableProject),
            eq(THIS_PLUGIN),
            eq(AbstractAotAnalysisMojo.NAME),
            configurationCaptor.capture()
        );
        Xpp3Dom enabled = configurationCaptor.getValue().getChild("enabled");
        assertNotNull(enabled);
        assertEquals(Boolean.TRUE.toString(), enabled.getValue());
    }

    @Test
    void buildRunArgumentsIsTodaysCommandLineWhenClassDataSharingIsOff() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(javaExecutable(), "-Xmx256m", "-Dmicronaut.environments=dev", "-classpath",
            output + File.pathSeparator + dependency, "-XX:TieredStopAtLevel=1", "-Dcom.sun.management.jmxremote",
            "com.example.Application", "--verbose"), args);
    }

    @Test
    void buildRunArgumentsPutsTheDependencyJarsFirstWhenClassDataSharingIsOn() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency);
        setField(mojo, "classDataSharingSupport", new ClassDataSharingSupport(new SystemStreamLog(), tempDir.resolve("app/target/mn-cds"),
            javaExecutable(), Map.of(), new ClassDataSharingSupport.Jdk("/jdks/25", "25.0.4.1", 25)));

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(javaExecutable(), args.get(0));
        assertTrue(args.get(1).startsWith("-XX:DumpLoadedClassList=" + tempDir.resolve("app/target/mn-cds")), args.toString());
        assertEquals(List.of("-Xmx256m", "-Dmicronaut.environments=dev", "-classpath", dependency + File.pathSeparator + output,
            "-XX:TieredStopAtLevel=1", "-Dcom.sun.management.jmxremote", "com.example.Application", "--verbose"), args.subList(2, args.size()));
    }

    @Test
    void aRestartKeepsTheClassListOfARecordingLaunchAndStartsTheDump() throws Exception {
        Path recorded = startRecordingLaunch();
        RunMojo mojo = recordingMojo;

        invoke(mojo, "killProcess");
        supportOf(mojo).awaitBackgroundWork();

        assertTrue(Files.isRegularFile(sibling(recorded, ".classlist")));
        // the test java does not exist, so the dump fails and says so
        assertTrue(Files.isRegularFile(sibling(recorded, ".failed")));
    }

    @Test
    void ctrlCKeepsTheClassListOfARecordingLaunchWithoutStartingTheDump() throws Exception {
        Path recorded = startRecordingLaunch();
        RunMojo mojo = recordingMojo;

        invoke(mojo, "stopOnShutdown");
        supportOf(mojo).awaitBackgroundWork();

        assertTrue(Files.isRegularFile(sibling(recorded, ".classlist")));
        assertFalse(Files.exists(sibling(recorded, ".failed")));
    }

    private Path startRecordingLaunch() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        recordingMojo = mojoForBuildRunArguments(output, dependency);
        var support = new ClassDataSharingSupport(new SystemStreamLog(), tempDir.resolve("app/target/mn-cds"),
            tempDir.resolve("no-such-java").toString(), Map.of(), new ClassDataSharingSupport.Jdk("/jdks/25", "25.0.4.1", 25));
        setField(recordingMojo, "classDataSharingSupport", support);
        List<String> args = invokeBuildRunArguments(recordingMojo);
        Path recorded = Path.of(args.get(1).substring("-XX:DumpLoadedClassList=".length()));
        Files.writeString(recorded, "java/lang/Object id: 1\n");
        var process = new ClassDataSharingSupportTest.FakeProcess(143);
        setField(recordingMojo, "process", process);
        support.launched(process);
        return recorded;
    }

    private static ClassDataSharingSupport supportOf(RunMojo mojo) throws Exception {
        Field field = RunMojo.class.getDeclaredField("classDataSharingSupport");
        field.setAccessible(true);
        return (ClassDataSharingSupport) field.get(mojo);
    }

    private static Path sibling(Path recorded, String suffix) {
        return Path.of(recorded.toString().replace(".recording", suffix));
    }

    private static void invoke(RunMojo mojo, String methodName) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod(methodName);
        method.setAccessible(true);
        method.invoke(mojo);
    }

    private RunMojo mojoForBuildRunArguments(Path output, Path dependency) throws Exception {
        MavenSession mavenSession = mock(MavenSession.class);
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        var runnableProject = new MavenProject();
        var build = new Build();
        build.setOutputDirectory(output.toString());
        build.setDirectory(output.getParent().toString());
        runnableProject.setBuild(build);
        var userProperties = new Properties();
        userProperties.setProperty("micronaut.environments", "dev");
        when(mavenSession.getAllProjects()).thenReturn(List.of(runnableProject));
        when(mavenSession.getUserProperties()).thenReturn(userProperties);
        when(mavenSession.getSystemProperties()).thenReturn(new Properties());
        RunMojo mojo = new RunMojo(
            mavenSession,
            mock(BuildPluginManager.class),
            mock(ProjectBuilder.class),
            toolchainManager,
            mock(CompilerService.class),
            mock(ExecutorService.class),
            mock(DependencyResolutionService.class)
        );
        setField(mojo, "runnableProject", runnableProject);
        setField(mojo, "targetDirectory", output.getParent().toFile());
        setField(mojo, "classpath", dependency.toString());
        setField(mojo, "jvmArguments", "-Xmx256m");
        setField(mojo, "appArguments", "--verbose");
        setField(mojo, "mainClass", "com.example.Application");
        return mojo;
    }

    private static String javaExecutable() {
        return MojoUtils.findJavaExecutable(mock(ToolchainManager.class), mock(MavenSession.class));
    }

    private static Path zip(Path file) throws Exception {
        Files.createDirectories(file.getParent());
        try (var zip = new ZipOutputStream(Files.newOutputStream(file))) {
            zip.putNextEntry(new ZipEntry("io/acme/Dependency.class"));
            zip.closeEntry();
        }
        return file;
    }

    @SuppressWarnings("unchecked")
    private static List<String> invokeBuildRunArguments(RunMojo mojo) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod("buildRunArguments");
        method.setAccessible(true);
        return (List<String>) method.invoke(mojo);
    }

    private static void setField(RunMojo mojo, String fieldName, Object value) throws Exception {
        Field field = RunMojo.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(mojo, value);
    }

    private static void invokeRunAotIfNeeded(RunMojo mojo) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod("runAotIfNeeded");
        method.setAccessible(true);
        method.invoke(mojo);
    }
}
