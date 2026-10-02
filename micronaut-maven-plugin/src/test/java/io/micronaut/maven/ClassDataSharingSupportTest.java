package io.micronaut.maven;

import io.micronaut.maven.ClassDataSharingSupport.Jdk;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.tools.ToolProvider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ClassDataSharingSupportTest {

    private static final Jdk JDK_25 = new Jdk("/jdks/25", "25.0.4.1", 25);
    private static final String MAIN_CLASS = "com.example.Application";

    @TempDir
    Path tempDir;

    @Test
    void putsTheJarsFirstThenTheReactorOutputsThenTheOtherEntries() throws IOException {
        Path first = jar("a.jar", "com/acme/A.class");
        Path other = Files.createDirectories(tempDir.resolve("other-entry"));
        Path second = jar("b.jar", "com/acme/B.class");
        Path app = classes("app", "com/example/Application.class");
        Path lib = classes("lib", "com/example/Lib.class");
        var launch = new Launch(List.of(app, lib), List.of(first, other, second));

        List<String> result = support(JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies);

        assertEquals(join(first, second, app, lib, other), classpathOf(result));
        assertTrue(result.get(1).startsWith("-XX:DumpLoadedClassList="), result.toString());
        assertEquals(launch.args().subList(launch.classpathIndex + 1, launch.args().size()), result.subList(result.indexOf(join(first, second, app, lib, other)) + 1, result.size()));
    }

    @Test
    void aDuplicateClassKeepsTodaysArgumentsWithAWarning() throws IOException {
        Path dependency = jar("dependency.jar", "com/example/Shared.class");
        Path app = classes("app", "com/example/Application.class", "com/example/Shared.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        var log = new CapturingLog();

        List<String> result = support(log, JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies);

        assertSame(launch.args(), result);
        assertTrue(log.contains("[warn]", "com/example/Shared.class"), log.lines.toString());
        assertTrue(log.contains("[warn]", dependency.toString()), log.lines.toString());
    }

    @Test
    void aDuplicateLogbackXmlKeepsTodaysArgumentsWithAWarning() throws IOException {
        Path dependency = jar("dependency.jar", "logback.xml", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class", "logback.xml");
        var launch = new Launch(List.of(app), List.of(dependency));
        var log = new CapturingLog();

        List<String> result = support(log, JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies);

        assertSame(launch.args(), result);
        assertTrue(log.contains("[warn]", "logback.xml"), log.lines.toString());
    }

    @Test
    void aFileOfANonJarEntryThatAJarAlsoHasIsADuplicate() throws IOException {
        Path dependency = jar("dependency.jar", "application.yml");
        Path other = classes("other-entry", "application.yml");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(other, dependency));

        List<String> result = support(JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies);

        assertSame(launch.args(), result);
    }

    @Test
    void aDamagedEntryIndexIsReadFromTheJarsAgain() throws IOException {
        Path dependency = jar("dependency.jar", "logback.xml");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        launch.prepare(support(JDK_25));
        Path index;
        try (var files = Files.list(tempDir.resolve("mn-cds"))) {
            index = files.filter(file -> file.toString().endsWith(".entries")).findFirst().orElseThrow();
        }
        Files.writeString(index, "not an index\n");
        classes("app", "logback.xml");

        assertSame(launch.args(), launch.prepare(support(JDK_25)));
    }

    @Test
    void mergedAndDescriptiveFilesAreNotDuplicates() throws IOException {
        String beanReferences = "META-INF/micronaut/io.micronaut.inject.BeanDefinitionReference/";
        Path dependency = jar("dependency.jar", beanReferences + "io.acme.$Bean$Definition$Reference",
            "META-INF/services/io.micronaut.core.type.TypeInformationProvider", "META-INF/MANIFEST.MF", "module-info.class",
            "META-INF/versions/21/com/acme/A.class", "META-INF/LICENSE.txt", "META-INF/maven/io.acme/acme/pom.properties");
        Path app = classes("app", beanReferences + "com.example.$Bean$Definition$Reference",
            "META-INF/services/io.micronaut.core.type.TypeInformationProvider", "META-INF/MANIFEST.MF", "module-info.class",
            "META-INF/versions/21/com/acme/A.class", "META-INF/LICENSE.txt", "META-INF/maven/io.acme/acme/pom.properties");
        var launch = new Launch(List.of(app), List.of(dependency));
        var log = new CapturingLog();

        List<String> result = support(log, JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies);

        assertEquals(join(dependency, app), classpathOf(result));
        assertFalse(log.contains("[warn]", ""), log.lines.toString());
    }

    @Test
    void theKeyChangesWithTheJarsTheJdkTheRootModulesAndTheHeapSize() throws IOException {
        Path jar = jar("a.jar", "com/acme/A.class");
        List<Path> jars = List.of(jar);
        Set<String> modules = Set.of("jdk.management.agent");
        List<String> vmOptions = List.of("-XX:TieredStopAtLevel=1");
        String key = ClassDataSharingSupport.key(JDK_25, modules, List.of(), vmOptions, jars);

        assertEquals(key, ClassDataSharingSupport.key(JDK_25, modules, List.of(), vmOptions, jars));
        assertNotEquals(key, ClassDataSharingSupport.key(new Jdk("/jdks/other-25", "25.0.4.1", 25), modules, List.of(), vmOptions, jars));
        assertNotEquals(key, ClassDataSharingSupport.key(new Jdk("/jdks/25", "25.0.5", 25), modules, List.of(), vmOptions, jars));
        assertNotEquals(key, ClassDataSharingSupport.key(JDK_25, Set.of(), List.of(), vmOptions, jars));
        assertNotEquals(key, ClassDataSharingSupport.key(JDK_25, modules, List.of(), List.of("-XX:TieredStopAtLevel=1", "-Xmx64g"), jars));
        assertNotEquals(key, ClassDataSharingSupport.key(JDK_25, modules, List.of("--add-opens=java.base/java.lang=ALL-UNNAMED"), vmOptions, jars));

        FileTime modified = Files.getLastModifiedTime(jar);
        Files.setLastModifiedTime(jar, FileTime.fromMillis(modified.toMillis() + 60_000));
        String touched = ClassDataSharingSupport.key(JDK_25, modules, List.of(), vmOptions, jars);
        assertNotEquals(key, touched);

        Files.write(jar, new byte[]{1, 2, 3}, StandardOpenOption.APPEND);
        Files.setLastModifiedTime(jar, FileTime.fromMillis(modified.toMillis() + 60_000));
        assertNotEquals(touched, ClassDataSharingSupport.key(JDK_25, modules, List.of(), vmOptions, jars));
    }

    @Test
    void theArchiveOfALaunchDependsOnTheToolchainJdk() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));

        String withJdk25 = support(JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies).get(1);
        String withAnotherBuild = support(new Jdk("/jdks/25", "25.0.5+1", 25)).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies).get(1);

        assertNotEquals(withJdk25, withAnotherBuild);
    }

    @Test
    void rootModulesAreDerivedFromTheLaunchAndTheEnvironment() {
        assertEquals(Set.of("jdk.management.agent"), ClassDataSharingSupport.rootModules(List.of("-Dcom.sun.management.jmxremote")));
        assertEquals(Set.of("jdk.management.agent"), ClassDataSharingSupport.rootModules(List.of("-Dcom.sun.management.foo=bar")));
        assertEquals(Set.of("java.instrument"), ClassDataSharingSupport.rootModules(List.of("-javaagent:/agents/agent.jar=opt")));
        assertEquals(Set.of("java.sql", "jdk.httpserver", "jdk.jfr"),
            ClassDataSharingSupport.rootModules(List.of("--add-modules=jdk.httpserver,java.sql", "--add-modules", "jdk.jfr")));
        assertEquals(Set.of(), ClassDataSharingSupport.rootModules(List.of("-Dmn.jvmArgs=-Dcom.sun.management.jmxremote", "-agentlib:jdwp=transport=dt_socket")));

        List<String> options = ClassDataSharingSupport.effectiveOptions(
            Map.of("JAVA_TOOL_OPTIONS", "-javaagent:/agents/agent.jar -Dcom.sun.management.jmxremote.port=9010"), List.of("-XX:TieredStopAtLevel=1"));
        assertEquals(Set.of("java.instrument", "jdk.management.agent"), ClassDataSharingSupport.rootModules(options));
        assertEquals(Set.of("jdk.httpserver"), ClassDataSharingSupport.rootModules(
            ClassDataSharingSupport.effectiveOptions(Map.of("JDK_JAVA_OPTIONS", "--add-modules=jdk.httpserver"), List.of())));
    }

    @Test
    void theRootModulesFromJavaToolOptionsChangeTheArchiveOfALaunch() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));

        String plain = support(JDK_25).prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies).get(1);
        String withAgent = new ClassDataSharingSupport(new CapturingLog(), tempDir.resolve("mn-cds"), "java",
            Map.of("JAVA_TOOL_OPTIONS", "-javaagent:/agents/agent.jar"), JDK_25)
            .prepareLaunch(launch.args(), launch.classpathIndex, launch.mainClassIndex, launch.outputs, launch.dependencies).get(1);

        assertNotEquals(plain, withAgent);
    }

    @Test
    void theArchiveOptionsAreTheXxAndHeapOptions() {
        assertEquals(List.of("-XX:TieredStopAtLevel=1", "-Xmx1g", "-XX:+UseZGC", "--enable-preview"),
            ClassDataSharingSupport.archiveOptions(List.of("-XX:TieredStopAtLevel=1", "-Dfoo=bar", "-Xmx1g", "-agentlib:jdwp=x",
                "-XX:+UseZGC", "-XX:StartFlightRecording=duration=30s", "-Xlog:gc", "--enable-preview")));
        assertEquals(List.of("--add-opens=java.base/java.lang=ALL-UNNAMED", "--enable-native-access=ALL-UNNAMED"),
            ClassDataSharingSupport.moduleOptions(List.of("--add-opens", "java.base/java.lang=ALL-UNNAMED", "--add-modules=java.sql",
                "--enable-native-access=ALL-UNNAMED")));
    }

    @Test
    void statusZeroCtrlCAndAStopByThePluginKeepTheClassList() {
        assertTrue(ClassDataSharingSupport.keepsClassList(false, 0));
        assertTrue(ClassDataSharingSupport.keepsClassList(false, 130));
        assertTrue(ClassDataSharingSupport.keepsClassList(true, 143));
        assertTrue(ClassDataSharingSupport.keepsClassList(true, 1));
        assertFalse(ClassDataSharingSupport.keepsClassList(false, 1));
        assertFalse(ClassDataSharingSupport.keepsClassList(false, 143));
        assertFalse(ClassDataSharingSupport.keepsClassList(false, 137));
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 130})
    void aLaunchThatExitsWithZeroOrCtrlCKeepsItsTrimmedClassList(int status) throws IOException {
        Path recorded = recordAndEnd(new FakeProcess(status), false);

        Path classList = Path.of(recorded.toString().replace(".recording", ".classlist"));
        assertTrue(Files.isRegularFile(classList));
        assertEquals("java/lang/Object id: 1\ncom/acme/A id: 2\n", Files.readString(classList));
        assertFalse(Files.exists(recorded));
    }

    @Test
    void aLaunchThePluginStopsKeepsItsClassList() throws IOException {
        Path recorded = recordAndEnd(new FakeProcess(143), true);

        assertTrue(Files.isRegularFile(Path.of(recorded.toString().replace(".recording", ".classlist"))));
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 143})
    void aLaunchThatFailsOnItsOwnDiscardsItsClassList(int status) throws IOException {
        Path recorded = recordAndEnd(new FakeProcess(status), false);

        assertFalse(Files.exists(Path.of(recorded.toString().replace(".recording", ".classlist"))));
        assertFalse(Files.exists(recorded));
    }

    @Test
    void aClassListKeptOnShutdownIsDumpedByTheNextGoal() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        var support = new ClassDataSharingSupport(new CapturingLog(), tempDir.resolve("mn-cds"), tempDir.resolve("no-such-java").toString(), Map.of(), JDK_25);
        List<String> recording = launch.prepare(support);
        Path recorded = recordingFile(recording);
        Files.writeString(recorded, "java/lang/Object id: 1\n");
        var process = new FakeProcess(130);
        support.launched(process);

        support.shutdown();
        process.alive = false;
        support.launchEnded(process, true);
        support.awaitBackgroundWork();

        Path classList = Path.of(recorded.toString().replace(".recording", ".classlist"));
        assertTrue(Files.isRegularFile(classList));
        assertFalse(Files.exists(Path.of(recorded.toString().replace(".recording", ".failed"))));
        var log = new CapturingLog();
        var nextGoal = new ClassDataSharingSupport(log, tempDir.resolve("mn-cds"), tempDir.resolve("no-such-java").toString(), Map.of(), JDK_25);
        List<String> next = launch.prepare(nextGoal);
        nextGoal.awaitBackgroundWork();
        assertEquals("java", next.get(0));
        assertEquals(launch.args().get(1), next.get(1));
        assertTrue(log.contains("[debug]", "creating the CDS archive"), log.lines.toString());
    }

    @Test
    void belowJdk25TheArgumentsAreTodays() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        var log = new CapturingLog();
        var support = support(log, new Jdk("/jdks/21", "21.0.12", 21));

        List<String> first = launch.prepare(support);
        List<String> second = launch.prepare(support);

        assertSame(launch.args(), first);
        assertSame(launch.args(), second);
        assertTrue(first.stream().noneMatch(argument -> argument.contains("aot") || argument.startsWith("-XX:Shared")));
        assertEquals(1, log.lines.stream().filter(line -> line.startsWith("[info]") && line.contains("JDK 25")).count(), log.lines.toString());
    }

    @ParameterizedTest
    @ValueSource(strings = {"-Xshare:off", "-Xshare:auto", "-XX:SharedArchiveFile=/tmp/app.jsa", "-XX:SharedClassListFile=/tmp/app.classlist",
        "-XX:ArchiveClassesAtExit=/tmp/app.jsa", "-XX:+AutoCreateSharedArchive", "-XX:DumpLoadedClassList=/tmp/app.classlist",
        "-XX:AOTCache=/tmp/app.aot", "-XX:AOTCacheOutput=/tmp/app.aot", "-XX:AOTMode=record", "--limit-modules=java.base",
        "--upgrade-module-path=/tmp/modules", "--patch-module=java.base=/tmp/patch", "--module-path=/tmp/modules", "@/tmp/jvm.args",
        "-XX:VMOptionsFile=/tmp/vm.options"})
    void jvmArgumentsThatManageCdsOrTheModuleGraphKeepTodaysArguments(String option) throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency), option);

        assertSame(launch.args(), launch.prepare(support(JDK_25)));
    }

    @Test
    void cdsOptionsInTheEnvironmentKeepTodaysArguments() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        var support = new ClassDataSharingSupport(new CapturingLog(), tempDir.resolve("mn-cds"), "java",
            Map.of("JDK_JAVA_OPTIONS", "-XX:AOTCache=/tmp/app.aot"), JDK_25);

        assertSame(launch.args(), launch.prepare(support));
    }

    @Test
    void aLaunchWithAnArchiveGetsItAndTheLogSelectionBeforeTheUserJvmArguments() throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency), "-Xlog:cds=info");
        var log = new CapturingLog();
        var support = support(log, JDK_25);
        String recordingFlag = launch.prepare(support).get(1);
        Path archive = Path.of(recordingFile(List.of("java", recordingFlag)).toString().replace(".recording", ".jsa"));
        Files.write(archive, new byte[]{1});

        List<String> result = launch.prepare(support);

        assertEquals(List.of("java", "-XX:SharedArchiveFile=" + archive, "-Xlog:cds*=off,aot*=off", "-Xlog:cds=info"), result.subList(0, 4));
        assertEquals(join(dependency, app), classpathOf(result));
        assertTrue(log.contains("[debug]", "without a probe"), log.lines.toString());
    }

    /**
     * Runs real launches, a real dump and a real probe with the JDK that runs the tests. Without compressed oops, an
     * archive is only usable when the dump got the launch's {@code -XX:} options as well.
     */
    @ParameterizedTest
    @ValueSource(strings = {"-XX:TieredStopAtLevel=1", "-XX:-UseCompressedOops"})
    void recordsThenDumpsInTheBackgroundThenLaunchesWithTheArchiveWithoutAProbe(String vmOption) throws Exception {
        String java = Path.of(System.getProperty("java.home"), "bin", File.separatorChar == '\\' ? "java.exe" : "java").toString();
        Path dependency = compiledJar("greeter.jar", "com/acme/Greeter.java",
            "package com.acme; public final class Greeter { public static String greet() { return \"Hello from the dependency\"; } }");
        Path app = tempDir.resolve("app/target/classes");
        compile(tempDir.resolve("app/src"), "com/example/Application.java",
            "package com.example; public class Application { public static void main(String[] args) { System.out.println(com.acme.Greeter.greet()); } }",
            app, dependency.toString());
        Path directory = tempDir.resolve("app/target/mn-cds");
        var log = new CapturingLog();
        var support = new ClassDataSharingSupport(log, directory, java, Map.of());

        // the first launch records its class list, and the dump starts when it exits
        var recordingLaunch = new Launch(java, List.of(app), List.of(dependency), vmOption);
        List<String> recording = recordingLaunch.prepare(support);
        assertTrue(recording.get(1).startsWith("-XX:DumpLoadedClassList="), recording.toString());
        Process first = start(recording, tempDir.resolve("first.out"));
        support.launched(first);
        assertEquals(0, first.waitFor());
        support.launchEnded(first, false);

        // a launch during the dump runs without an archive and records nothing
        List<String> duringDump = recordingLaunch.prepare(support);
        assertEquals(recordingLaunch.args().subList(1, recordingLaunch.classpathIndex), duringDump.subList(1, recordingLaunch.classpathIndex));
        assertEquals(join(dependency, app), classpathOf(duringDump));
        support.awaitBackgroundWork();
        Path archive;
        try (var files = Files.list(directory)) {
            archive = files.filter(file -> file.toString().endsWith(".jsa")).findFirst().orElseThrow(() -> new AssertionError(log.lines.toString()));
        }

        // the next launch uses the archive, without a probe; the CDS warnings that the user option turns back on
        // would report a dump whose module graph differs from the launch's (jdk.management.agent, for jmxremote)
        Path classLoading = tempDir.resolve("class-load.log");
        var archivedLaunch = new Launch(java, List.of(app), List.of(dependency), vmOption, "-Xlog:cds=warning", "-Xlog:class+load:file=" + classLoading);
        List<String> withArchive = archivedLaunch.prepare(support);
        assertEquals(List.of("-XX:SharedArchiveFile=" + archive, "-Xlog:cds*=off,aot*=off", vmOption), withArchive.subList(1, 4));
        assertTrue(log.contains("[debug]", "without a probe"), log.lines.toString());
        Path output = tempDir.resolve("third.out");
        Process third = start(withArchive, output);
        assertEquals(0, third.waitFor());
        String printed = Files.readString(output);
        assertTrue(printed.contains("Hello from the dependency"), printed);
        assertFalse(printed.contains("[cds") || printed.contains("[aot"), printed);
        String loaded = Files.readString(classLoading);
        assertTrue(loaded.contains("com.acme.Greeter source: shared objects file"), loaded);
        assertTrue(loaded.lines().anyMatch(line -> line.contains("com.example.Application source: file:")), loaded);
        String dumpLog = Files.readString(Path.of(archive.toString().replace(".jsa", ".log")));
        assertEquals(1, dumpLog.lines().filter(line -> line.contains(" version \"")).count(), dumpLog);
    }

    @Test
    void trimmingKeepsCompleteLinesOnly() throws IOException {
        Path recorded = Files.writeString(tempDir.resolve("list.recording"), "a id: 1\nb id: 2\nc i");
        Path target = tempDir.resolve("list.classlist");

        assertTrue(ClassDataSharingSupport.trimToLastCompleteLine(recorded, target));
        assertEquals("a id: 1\nb id: 2\n", Files.readString(target));

        Files.writeString(recorded, "partial");
        assertFalse(ClassDataSharingSupport.trimToLastCompleteLine(recorded, tempDir.resolve("other.classlist")));
    }

    @Test
    void parsesTheJdkFromShowSettings() {
        var jdk = Jdk.parse(List.of("Property settings:", "    java.home = /jdks/25", "    java.specification.version = 25",
            "    java.vm.version = 25.0.4.1", "    line.separator = \\n"));

        assertEquals(new Jdk("/jdks/25", "25.0.4.1", 25), jdk.orElseThrow());
        assertTrue(Jdk.parse(List.of("java.home = /jdks/25")).isEmpty());
    }

    private Path recordAndEnd(FakeProcess process, boolean stoppedByPlugin) throws IOException {
        Path dependency = jar("a.jar", "com/acme/A.class");
        Path app = classes("app", "com/example/Application.class");
        var launch = new Launch(List.of(app), List.of(dependency));
        var support = new ClassDataSharingSupport(new CapturingLog(), tempDir.resolve("mn-cds"), tempDir.resolve("no-such-java").toString(), Map.of(), JDK_25);
        List<String> recording = launch.prepare(support);
        Path recorded = recordingFile(recording);
        Files.writeString(recorded, "java/lang/Object id: 1\ncom/acme/A id: 2\ncom/acme/B i");
        support.launched(process);
        process.alive = false;
        support.launchEnded(process, stoppedByPlugin);
        support.awaitBackgroundWork();
        return recorded;
    }

    private Path compiledJar(String name, String sourceFile, String source) throws IOException {
        Path classes = tempDir.resolve(name + "-classes");
        compile(tempDir.resolve(name + "-src"), sourceFile, source, classes, null);
        Path jar = tempDir.resolve("repository").resolve(name);
        Files.createDirectories(jar.getParent());
        try (var zip = new ZipOutputStream(Files.newOutputStream(jar)); var files = Files.walk(classes)) {
            for (Path file : files.filter(Files::isRegularFile).toList()) {
                zip.putNextEntry(new ZipEntry(classes.relativize(file).toString().replace(File.separatorChar, '/')));
                zip.write(Files.readAllBytes(file));
                zip.closeEntry();
            }
        }
        return jar;
    }

    private static void compile(Path sources, String sourceFile, String source, Path output, String classpath) throws IOException {
        Path file = sources.resolve(sourceFile);
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
        Files.createDirectories(output);
        var arguments = new ArrayList<>(List.of("-d", output.toString()));
        if (classpath != null) {
            arguments.addAll(List.of("-cp", classpath));
        }
        arguments.add(file.toString());
        assertEquals(0, ToolProvider.getSystemJavaCompiler().run(null, null, null, arguments.toArray(String[]::new)));
    }

    private static Process start(List<String> args, Path output) throws IOException {
        return new ProcessBuilder(args).redirectErrorStream(true).redirectOutput(output.toFile()).start();
    }

    private static Path recordingFile(List<String> args) {
        String flag = args.get(1);
        assertTrue(flag.startsWith("-XX:DumpLoadedClassList="), args.toString());
        return Path.of(flag.substring("-XX:DumpLoadedClassList=".length()));
    }

    private ClassDataSharingSupport support(Jdk jdk) {
        return support(new CapturingLog(), jdk);
    }

    private ClassDataSharingSupport support(CapturingLog log, Jdk jdk) {
        return new ClassDataSharingSupport(log, tempDir.resolve("mn-cds"), "java", Map.of(), jdk);
    }

    private Path jar(String name, String... entries) throws IOException {
        Path jar = tempDir.resolve("repository").resolve(name);
        Files.createDirectories(jar.getParent());
        try (var zip = new ZipOutputStream(Files.newOutputStream(jar))) {
            for (String entry : entries) {
                zip.putNextEntry(new ZipEntry(entry));
                zip.write(entry.getBytes(StandardCharsets.UTF_8));
                zip.closeEntry();
            }
        }
        return jar;
    }

    private Path classes(String project, String... files) throws IOException {
        Path classes = tempDir.resolve(project).resolve("target/classes");
        Files.createDirectories(classes);
        for (String file : files) {
            Path path = classes.resolve(file);
            Files.createDirectories(path.getParent());
            Files.writeString(path, file);
        }
        return classes;
    }

    private static String join(Path... paths) {
        return String.join(File.pathSeparator, Arrays.stream(paths).map(Path::toString).toList());
    }

    private static String classpathOf(List<String> args) {
        return args.get(args.indexOf("-classpath") + 1);
    }

    /**
     * The arguments that {@code mn:run} builds today, in the shape of {@code RunMojo#buildRunArguments}.
     */
    private static final class Launch {

        private final List<String> outputs;
        private final String dependencies;
        private final int classpathIndex;
        private final int mainClassIndex;
        private final List<String> args;

        private Launch(List<Path> outputs, List<Path> dependencies, String... jvmArguments) {
            this("java", outputs, dependencies, jvmArguments);
        }

        private Launch(String java, List<Path> outputs, List<Path> dependencies, String... jvmArguments) {
            this.outputs = outputs.stream().map(Path::toString).toList();
            this.dependencies = String.join(File.pathSeparator, dependencies.stream().map(Path::toString).toList());
            var arguments = new ArrayList<String>();
            arguments.add(java);
            arguments.addAll(List.of(jvmArguments));
            arguments.add("-Dmicronaut.environments=dev");
            arguments.add("-classpath");
            this.classpathIndex = arguments.size();
            arguments.add(String.join(File.pathSeparator, this.outputs) + File.pathSeparator + this.dependencies);
            arguments.add("-XX:TieredStopAtLevel=1");
            arguments.add("-Dcom.sun.management.jmxremote");
            this.mainClassIndex = arguments.size();
            arguments.add(MAIN_CLASS);
            arguments.add("--verbose");
            this.args = List.copyOf(arguments);
        }

        List<String> args() {
            return args;
        }

        List<String> prepare(ClassDataSharingSupport support) {
            return support.prepareLaunch(args, classpathIndex, mainClassIndex, outputs, dependencies);
        }
    }

    static final class CapturingLog extends SystemStreamLog {

        final List<String> lines = new CopyOnWriteArrayList<>();

        @Override
        public boolean isDebugEnabled() {
            return true;
        }

        @Override
        public void debug(CharSequence content) {
            lines.add("[debug] " + content);
        }

        @Override
        public void info(CharSequence content) {
            lines.add("[info] " + content);
        }

        @Override
        public void warn(CharSequence content) {
            lines.add("[warn] " + content);
        }

        boolean contains(String level, String text) {
            return lines.stream().anyMatch(line -> line.startsWith(level) && line.contains(text));
        }
    }

    static final class FakeProcess extends Process {

        private final int exitValue;
        private volatile boolean alive = true;

        FakeProcess(int exitValue) {
            this.exitValue = exitValue;
        }

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public InputStream getErrorStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public int waitFor() {
            return exitValue;
        }

        @Override
        public int exitValue() {
            if (alive) {
                throw new IllegalThreadStateException();
            }
            return exitValue;
        }

        @Override
        public boolean isAlive() {
            return alive;
        }

        @Override
        public void destroy() {
            alive = false;
        }
    }
}
