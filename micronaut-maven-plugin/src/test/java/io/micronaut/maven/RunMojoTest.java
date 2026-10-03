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
import java.util.Set;
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

    private static final String JMXREMOTE = "-Dcom.sun.management.jmxremote";
    private static final String MAIN_CLASS = "com.example.Application";
    private static final String OUTPUT_DIRECTORY = "/project/target/classes";
    private static final String DEPENDENCIES = "/repo/micronaut-inject.jar";
    private static final ClassDataSharingSupport.Jdk JDK_25 = new ClassDataSharingSupport.Jdk("/jdks/25", "25.0.4.1", 25);

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
    void runArgumentsDoNotStartTheJmxAgentByDefault() throws Exception {
        RunMojo mojo = runMojoForArguments(new Properties());

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    @Test
    void runArgumentsStartTheJmxAgentWhenTheJvmArgumentsAskForIt() throws Exception {
        RunMojo mojo = runMojoForArguments(new Properties());
        setField(mojo, "jvmArguments", JMXREMOTE);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            JMXREMOTE,
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    @Test
    void runArgumentsStartTheJmxAgentWhenAUserPropertyAsksForIt() throws Exception {
        var userProperties = new Properties();
        // Maven gives a -D without a value the value "true"
        userProperties.setProperty("com.sun.management.jmxremote", "true");
        RunMojo mojo = runMojoForArguments(userProperties);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(
            JMXREMOTE + "=true",
            "-classpath", OUTPUT_DIRECTORY + File.pathSeparator + DEPENDENCIES,
            "-XX:TieredStopAtLevel=1",
            MAIN_CLASS
        ), args.subList(1, args.size()));
    }

    @Test
    void buildRunArgumentsIsTodaysCommandLineWhenClassDataSharingIsOff() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency);

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(List.of(javaExecutable(), "-Xmx256m", "-Dmicronaut.environments=dev", "-classpath",
            output + File.pathSeparator + dependency, "-XX:TieredStopAtLevel=1",
            "com.example.Application", "--verbose"), args);
    }

    @Test
    void buildRunArgumentsPutsTheDependencyJarsFirstWhenClassDataSharingIsOn() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency);
        setField(mojo, "classDataSharingSupport", classDataSharingSupport(javaExecutable()));

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(javaExecutable(), args.get(0));
        // the launch adds no -Dcom.sun.management… property, so the archive has no root module to add
        assertEquals(recordingFlag(Set.of(), dependency), args.get(1));
        assertEquals(List.of("-Xmx256m", "-Dmicronaut.environments=dev", "-classpath", dependency + File.pathSeparator + output,
            "-XX:TieredStopAtLevel=1", "com.example.Application", "--verbose"), args.subList(2, args.size()));
    }

    @Test
    void theArchiveFollowsAJmxAgentRequestedThroughTheJvmArguments() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency);
        setField(mojo, "jvmArguments", "-Xmx256m " + JMXREMOTE);
        setField(mojo, "classDataSharingSupport", classDataSharingSupport(javaExecutable()));

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(recordingFlag(Set.of("jdk.management.agent"), dependency), args.get(1));
    }

    @Test
    void theArchiveFollowsAJmxAgentRequestedThroughAUserProperty() throws Exception {
        Path dependency = zip(tempDir.resolve("repository/dependency.jar"));
        Path output = Files.createDirectories(tempDir.resolve("app/target/classes"));
        RunMojo mojo = mojoForBuildRunArguments(output, dependency, Map.of("com.sun.management.jmxremote", "true"));
        setField(mojo, "classDataSharingSupport", classDataSharingSupport(javaExecutable()));

        List<String> args = invokeBuildRunArguments(mojo);

        assertEquals(recordingFlag(Set.of("jdk.management.agent"), dependency), args.get(1));
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
        var support = classDataSharingSupport(tempDir.resolve("no-such-java").toString());
        setField(recordingMojo, "classDataSharingSupport", support);
        List<String> args = invokeBuildRunArguments(recordingMojo);
        Path recorded = Path.of(args.get(1).substring("-XX:DumpLoadedClassList=".length()));
        Files.writeString(recorded, "java/lang/Object id: 1\n");
        var process = new ClassDataSharingSupportTest.FakeProcess(143);
        setField(recordingMojo, "process", process);
        support.launched(process);
        return recorded;
    }

    private ClassDataSharingSupport classDataSharingSupport(String javaExecutable) {
        return new ClassDataSharingSupport(new SystemStreamLog(), tempDir.resolve("app/target/mn-cds"), javaExecutable, Map.of(), JDK_25);
    }

    /**
     * @return the flag of a launch that records the class list of the archive with these root modules, for the JVM
     * options of {@link #mojoForBuildRunArguments(Path, Path, Map)}
     */
    private String recordingFlag(Set<String> rootModules, Path dependency) throws Exception {
        String key = ClassDataSharingSupport.key(JDK_25, rootModules, List.of(), List.of("-Xmx256m", "-XX:TieredStopAtLevel=1"),
            List.of(dependency));
        return "-XX:DumpLoadedClassList=" + tempDir.resolve("app/target/mn-cds").resolve(key + ".recording");
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

    private static RunMojo runMojoForArguments(Properties userProperties) throws Exception {
        MavenSession mavenSession = mock(MavenSession.class);
        ToolchainManager toolchainManager = mock(ToolchainManager.class);
        when(toolchainManager.getToolchainFromBuildContext("jdk", mavenSession)).thenReturn(null);
        MavenProject runnableProject = new MavenProject();
        runnableProject.getBuild().setOutputDirectory(OUTPUT_DIRECTORY);
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
        setField(mojo, "targetDirectory", new File("/project/target"));
        setField(mojo, "classpath", DEPENDENCIES);
        setField(mojo, "mainClass", MAIN_CLASS);
        return mojo;
    }

    private RunMojo mojoForBuildRunArguments(Path output, Path dependency) throws Exception {
        return mojoForBuildRunArguments(output, dependency, Map.of());
    }

    private RunMojo mojoForBuildRunArguments(Path output, Path dependency, Map<String, String> moreUserProperties) throws Exception {
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
        userProperties.putAll(moreUserProperties);
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

    @SuppressWarnings("unchecked")
    private static List<String> invokeBuildRunArguments(RunMojo mojo) throws Exception {
        Method method = RunMojo.class.getDeclaredMethod("buildRunArguments");
        method.setAccessible(true);
        return (List<String>) method.invoke(mojo);
    }
}
