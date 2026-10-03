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
package io.micronaut.maven;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.plexus.util.cli.CommandLineUtils;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Keeps the classes of the dependency JARs of {@code mn:run} in a static CDS archive.
 *
 * <p>While it is enabled, every launch puts the regular JAR files of the resolved class path first, then the output
 * directories of the reactor, then any other dependency entry. Only the JARs go into the archive: the JDK refuses an
 * archive whose class path holds a non-empty directory. The first launch records the classes it loads with
 * {@code -XX:DumpLoadedClassList}; when it ends, the archive is dumped and validated once in the background, and later
 * launches use it until its key (the JDK, the JVM options that decide whether an archive can be used, and the path,
 * size and modification time of every JAR) changes.</p>
 *
 * <p>A launch keeps today's command line when the JDK is older than 25, when the JVM options manage CDS or the module
 * graph themselves, or when a file of a reactor output (or of another entry that moves) also exists in a JAR, because
 * the new order would change which copy the application sees.</p>
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 5.1.0
 */
final class ClassDataSharingSupport {

    /**
     * The directory, under the build directory of the runnable project, that holds the archives and their inputs.
     */
    static final String DIRECTORY_NAME = "mn-cds";

    private static final int MINIMUM_JDK = 25;
    private static final String LOG_OFF = "-Xlog:cds*=off,aot*=off";
    private static final String MANAGEMENT_AGENT_MODULE = "jdk.management.agent";
    private static final String INSTRUMENT_MODULE = "java.instrument";
    private static final String JFR_MODULE = "jdk.jfr";
    private static final String JVMCI_MODULE = "jdk.internal.vm.ci";
    private static final int CTRL_C_STATUS = 130;
    private static final String SHARED_ARCHIVE_FILE = "-XX:SharedArchiveFile=";
    private static final int FORMAT = 1;
    private static final int KEY_BYTES = 8;
    private static final int MAX_REPORTED_DUPLICATES = 5;
    private static final double MEGABYTE = 1_000_000d;
    private static final double NANOS_PER_SECOND = 1_000_000_000d;
    private static final Duration DUMP_TIMEOUT = Duration.ofMinutes(5);
    private static final Duration PROBE_TIMEOUT = Duration.ofMinutes(1);
    private static final Duration STALE_TEMPORARY = Duration.ofHours(1);
    private static final String ARCHIVE_SUFFIX = ".jsa";
    private static final String CLASS_LIST_SUFFIX = ".classlist";
    private static final String RECORDING_SUFFIX = ".recording";
    private static final String ENTRIES_SUFFIX = ".entries";
    private static final String FAILED_SUFFIX = ".failed";
    private static final String LOG_SUFFIX = ".log";
    private static final String TEMPORARY_SUFFIX = ".tmp";
    private static final String ADD_MODULES = "--add-modules";
    private static final String VERSION = "-version";
    private static final List<String> ENVIRONMENT_OPTIONS = List.of("JAVA_TOOL_OPTIONS", "JDK_JAVA_OPTIONS", "_JAVA_OPTIONS");
    private static final List<String> MODULE_OPTIONS = List.of("--add-opens", "--add-exports", "--add-reads", "--enable-native-access");
    private static final List<String> MODULE_GRAPH_OPTIONS = List.of("--limit-modules", "--upgrade-module-path", "--patch-module", "--module-path", "-p");
    private static final Set<String> CDS_FLAGS = Set.of("SharedArchiveFile", "SharedClassListFile", "ArchiveClassesAtExit",
        "AutoCreateSharedArchive", "DumpLoadedClassList", "AOTCache", "AOTCacheOutput", "AOTMode", "AOTConfiguration",
        "AOTClassLinking", "UseSharedSpaces", "RecordDynamicDumpInfo", "SharedBaseAddress");
    private static final Set<String> OPTION_FILE_FLAGS = Set.of("VMOptionsFile", "Flags");
    private static final Set<String> JFR_FLAGS = Set.of("StartFlightRecording", "FlightRecorderOptions");
    private static final Pattern NOTICE_FILE = Pattern.compile("(?i)(META-INF/)?(LICEN[CS]E|NOTICE|COPYRIGHT)([._-][^/]*)?");
    private static final Pattern ENABLE_JVMCI = Pattern.compile("\\bbool\\s+EnableJVMCI\\s*=\\s*true\\b");
    private static final Pattern SIGNATURE_FILE = Pattern.compile("(?i)META-INF/[^/]+\\.(SF|RSA|DSA|EC)");

    private final Log log;
    private final Path directory;
    private final String javaExecutable;
    private final Map<String, String> environment;
    private final Set<String> announcedKeys = new HashSet<>();
    private Jdk jdk;
    private boolean jdkRead;
    private boolean jdkReported;
    private boolean failureReported;
    private boolean staleTemporariesDeleted;
    private String currentKey;
    private String lastDuplicateWarning;
    private String entriesKey;
    private Map<String, Integer> entries;
    private Recording pendingRecording;
    private Recording recording;
    private ExecutorService executor;
    private Future<?> dump;
    private String dumpKey;
    private Process dumpProcess;
    private Path dumpTemporary;
    private boolean shuttingDown;

    /**
     * @param log the Maven log
     * @param directory the directory that holds the archives, under the build directory of the runnable project
     * @param javaExecutable the {@code java} that launches the application
     * @param environment the environment that the application inherits
     */
    ClassDataSharingSupport(Log log, Path directory, String javaExecutable, Map<String, String> environment) {
        this(log, directory, javaExecutable, environment, null);
    }

    /**
     * @param log the Maven log
     * @param directory the directory that holds the archives, under the build directory of the runnable project
     * @param javaExecutable the {@code java} that launches the application
     * @param environment the environment that the application inherits
     * @param jdk the JDK of {@code javaExecutable}, or {@code null} to read it from {@code javaExecutable} once
     */
    ClassDataSharingSupport(Log log, Path directory, String javaExecutable, Map<String, String> environment, Jdk jdk) {
        this.log = log;
        this.directory = directory.toAbsolutePath();
        this.javaExecutable = javaExecutable;
        this.environment = Map.copyOf(environment);
        this.jdk = jdk;
        this.jdkRead = jdk != null;
    }

    /**
     * Adapts the arguments of one launch.
     *
     * @param args the arguments that {@code mn:run} launches without this option: {@code java} first, then the JVM
     * options, {@code -classpath} and its value, more JVM options, the main class and the application arguments
     * @param classpathIndex the index of the class path value in {@code args}
     * @param mainClassIndex the index of the main class in {@code args}
     * @param reactorOutputs the output directories of the runnable project and of the reactor projects it depends on,
     * in the order of the class path
     * @param dependencyClasspath the resolved dependencies, joined with the path separator
     * @return {@code args} itself when this launch keeps today's command line, otherwise the adapted arguments
     */
    synchronized List<String> prepareLaunch(List<String> args, int classpathIndex, int mainClassIndex,
                                            List<String> reactorOutputs, String dependencyClasspath) {
        try {
            return prepare(args, classpathIndex, mainClassIndex, reactorOutputs, dependencyClasspath);
        } catch (RuntimeException e) {
            pendingRecording = null;
            if (!failureReported) {
                failureReported = true;
                log.warn("Class data sharing: could not prepare the launch (%s), so mn:run launches the application without it".formatted(e));
            }
            debug("could not prepare the launch: " + e);
            return args;
        }
    }

    private List<String> prepare(List<String> args, int classpathIndex, int mainClassIndex, List<String> reactorOutputs,
                                 String dependencyClasspath) {
        pendingRecording = null;
        finishEndedRecording();
        Optional<Jdk> launchJdk = jdk();
        if (launchJdk.isEmpty()) {
            return args;
        }
        if (launchJdk.get().feature() < MINIMUM_JDK) {
            if (!jdkReported) {
                jdkReported = true;
                log.info("Class data sharing needs JDK %d or later, and %s is JDK %d: mn:run launches the application without a CDS archive"
                    .formatted(MINIMUM_JDK, javaExecutable, launchJdk.get().feature()));
            }
            return args;
        }
        var launchOptions = new ArrayList<>(args.subList(1, classpathIndex - 1));
        launchOptions.addAll(args.subList(classpathIndex + 1, mainClassIndex));
        List<String> options = effectiveOptions(environment, launchOptions);
        Optional<String> managed = userManagedOption(options);
        if (managed.isPresent()) {
            debug("the JVM options contain %s, so they decide what CDS does: launching without changes".formatted(managed.get()));
            return args;
        }
        Layout layout = layout(dependencyClasspath, reactorOutputs);
        if (layout.jars().isEmpty()) {
            debug("the class path has no JAR to archive: launching without changes");
            return args;
        }
        Set<String> rootModules = rootModules(options);
        List<String> moduleOptions = moduleOptions(options);
        List<String> archiveOptions = archiveOptions(options);
        String key;
        List<Duplicate> duplicates;
        try {
            deleteStaleTemporaries();
            key = key(launchJdk.get(), rootModules, moduleOptions, archiveOptions, layout.jars());
            duplicates = findDuplicates(layout, entries(key, layout.jars()));
        } catch (IOException | UncheckedIOException e) {
            debug("could not read the class path (%s): launching without changes".formatted(e.getMessage()));
            return args;
        }
        currentKey = key;
        if (!duplicates.isEmpty()) {
            warnAboutDuplicates(duplicates);
            return args;
        }
        lastDuplicateWarning = null;
        String classpath = layout.classpath();
        var request = new DumpRequest(key, file(key, CLASS_LIST_SUFFIX), layout.jars(), rootModules, moduleOptions, archiveOptions, classpath);
        List<String> flags = flags(request);
        var result = new ArrayList<String>(args.size() + flags.size());
        result.add(args.get(0));
        result.addAll(flags);
        result.addAll(args.subList(1, classpathIndex));
        result.add(classpath);
        result.addAll(args.subList(classpathIndex + 1, args.size()));
        return result;
    }

    /**
     * Tells this support that the launch prepared last has started.
     *
     * @param process the application process
     */
    synchronized void launched(Process process) {
        if (pendingRecording != null) {
            pendingRecording.process = process;
            recording = pendingRecording;
            pendingRecording = null;
        }
    }

    /**
     * Tells this support that a launch has ended. When that launch recorded a class list, keeps the list if the plugin
     * stopped the application, or if it exited with status 0 or 130 (Ctrl+C), and then dumps the archive in the
     * background unless Maven is shutting down.
     *
     * @param process the application process
     * @param stoppedByPlugin whether the plugin stopped the application (on a restart or at the end of the goal)
     */
    synchronized void launchEnded(Process process, boolean stoppedByPlugin) {
        if (recording != null && recording.process == process && !process.isAlive()) {
            finishRecording(stoppedByPlugin);
        }
    }

    /**
     * Waits for an archive that is being dumped, so that a goal that ends on its own leaves it behind.
     */
    void awaitBackgroundWork() {
        Future<?> running;
        synchronized (this) {
            running = dump;
        }
        if (running != null && !running.isDone()) {
            log.info("Class data sharing: waiting for the CDS archive of the dependency JARs to be created");
            try {
                running.get();
            } catch (InterruptedException _) {
                Thread.currentThread().interrupt();
            } catch (ExecutionException e) {
                debug("the archive could not be created: " + e.getCause());
            }
        }
        synchronized (this) {
            if (executor != null) {
                executor.shutdown();
                executor = null;
            }
        }
    }

    /**
     * Called when Maven shuts down (for example on Ctrl+C): a class list recorded so far is kept for the next goal,
     * no new dump starts, and a running dump is stopped without delaying the exit.
     */
    void shutdown() {
        Process running;
        Path temporary;
        synchronized (this) {
            shuttingDown = true;
            running = dumpProcess;
            temporary = dumpTemporary;
        }
        if (running != null) {
            running.destroyForcibly();
            try {
                running.onExit().get(1, TimeUnit.SECONDS);
            } catch (InterruptedException _) {
                Thread.currentThread().interrupt();
            } catch (ExecutionException | TimeoutException _) {
                // the dump leaves a temporary file at worst, which the next goal deletes
            }
            deleteQuietly(temporary);
        }
    }

    /**
     * @param stoppedByPlugin whether the plugin stopped the application
     * @param exitStatus the exit status of the application, when it exited on its own
     * @return whether the class list that the launch recorded can be used for an archive
     */
    static boolean keepsClassList(boolean stoppedByPlugin, int exitStatus) {
        return stoppedByPlugin || exitStatus == 0 || exitStatus == CTRL_C_STATUS;
    }

    /**
     * @param environment the environment of the application
     * @param launchOptions the JVM options of the launch command
     * @return the JVM options in the order in which the JVM applies them: {@code JAVA_TOOL_OPTIONS},
     * {@code JDK_JAVA_OPTIONS}, the command line, then {@code _JAVA_OPTIONS}
     */
    static List<String> effectiveOptions(Map<String, String> environment, List<String> launchOptions) {
        var options = new ArrayList<String>();
        options.addAll(environmentOptions(environment, ENVIRONMENT_OPTIONS.get(0)));
        options.addAll(environmentOptions(environment, ENVIRONMENT_OPTIONS.get(1)));
        options.addAll(launchOptions);
        options.addAll(environmentOptions(environment, ENVIRONMENT_OPTIONS.get(2)));
        return options;
    }

    /**
     * @param options the effective JVM options
     * @return the first option that manages CDS, the JDK AOT cache or the module graph, or that reads more options
     * from a file, if any
     */
    static Optional<String> userManagedOption(List<String> options) {
        return joinOptionValues(options).stream().filter(ClassDataSharingSupport::managesCdsOrModuleGraph).findFirst();
    }

    /**
     * Derives the root modules that the JVM adds on its own, as the dump has to add them too: HotSpot adds
     * {@code jdk.management.agent} for any {@code -Dcom.sun.management…} property, {@code java.instrument} for a
     * {@code -javaagent:}, {@code jdk.jfr} for {@code -XX:StartFlightRecording} or {@code -XX:FlightRecorderOptions},
     * and the modules of every {@code --add-modules}.
     *
     * @param options the effective JVM options
     * @return the root modules, sorted
     */
    static Set<String> rootModules(List<String> options) {
        var modules = new TreeSet<String>();
        for (String option : joinOptionValues(options)) {
            if (option.startsWith("-Dcom.sun.management")) {
                modules.add(MANAGEMENT_AGENT_MODULE);
            } else if (option.startsWith("-javaagent:")) {
                modules.add(INSTRUMENT_MODULE);
            } else if (JFR_FLAGS.contains(String.valueOf(flagName(option)))) {
                modules.add(JFR_MODULE);
            } else if (option.startsWith(ADD_MODULES + "=")) {
                Arrays.stream(option.substring(ADD_MODULES.length() + 1).split(","))
                    .map(String::trim)
                    .filter(module -> !module.isEmpty())
                    .forEach(modules::add);
            }
        }
        return modules;
    }

    /**
     * @param options the effective JVM options
     * @return the module options other than {@code --add-modules}, each as one {@code --option=value} argument
     */
    static List<String> moduleOptions(List<String> options) {
        return joinOptionValues(options).stream()
            .filter(option -> MODULE_OPTIONS.stream().anyMatch(moduleOption -> option.startsWith(moduleOption + "=")))
            .toList();
    }

    /**
     * @param options JVM options
     * @return the options, with the value of each module option that takes it as the next argument joined to it with
     * {@code =}
     */
    static List<String> joinOptionValues(List<String> options) {
        var joined = new ArrayList<String>(options.size());
        String pending = null;
        for (String option : options) {
            if (pending != null) {
                joined.add(pending + "=" + option);
                pending = null;
            } else if (option.equals(ADD_MODULES) || MODULE_OPTIONS.contains(option) || MODULE_GRAPH_OPTIONS.contains(option)) {
                pending = option;
            } else {
                joined.add(option);
            }
        }
        if (pending != null) {
            joined.add(pending);
        }
        return joined;
    }

    /**
     * Selects the options that decide whether the JVM can map an archive: the {@code -XX:} options (the collector,
     * compressed oops and class pointers among them), the heap size, which turns compressed oops off when it is large,
     * and {@code --enable-preview}. The dump and the probe get them too. The JFR options are left out, so that the dump
     * does not start a recording; {@link #rootModules(List)} adds their module instead.
     *
     * @param options the effective JVM options
     * @return those options, in order
     */
    static List<String> archiveOptions(List<String> options) {
        return options.stream()
            .filter(option -> {
                String flag = flagName(option);
                return (flag != null && !JFR_FLAGS.contains(flag))
                    || option.startsWith("-Xmx") || option.startsWith("-Xms") || option.equals("--enable-preview");
            })
            .toList();
    }

    /**
     * Splits the class path in the order this option uses.
     *
     * @param dependencyClasspath the resolved dependencies, joined with the path separator
     * @param reactorOutputs the reactor output directories
     * @return the layout
     */
    static Layout layout(String dependencyClasspath, List<String> reactorOutputs) {
        var jars = new ArrayList<Path>();
        var others = new ArrayList<Path>();
        if (dependencyClasspath != null) {
            for (String entry : dependencyClasspath.split(Pattern.quote(File.pathSeparator))) {
                if (!entry.isEmpty()) {
                    Path path = Path.of(entry);
                    if (isJar(path)) {
                        jars.add(path);
                    } else {
                        others.add(path);
                    }
                }
            }
        }
        return new Layout(jars, reactorOutputs.stream().map(Path::of).toList(), others);
    }

    /**
     * @param jdk the JDK of the launch
     * @param rootModules the root modules
     * @param moduleOptions the other module options
     * @param archiveOptions the {@code -XX:} and heap options
     * @param jars the JARs of the archive, in class path order
     * @return the key of the archive
     * @throws IOException if a JAR cannot be read
     */
    static String key(Jdk jdk, Set<String> rootModules, List<String> moduleOptions, List<String> archiveOptions,
                      List<Path> jars) throws IOException {
        MessageDigest digest;
        try {
            digest = MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
        var text = new StringBuilder()
            .append("format=").append(FORMAT).append('\n')
            .append("java.home=").append(jdk.home()).append('\n')
            .append("java.vm.version=").append(jdk.vmVersion()).append('\n')
            .append("mode=plain\n")
            .append("modules=").append(String.join(",", rootModules)).append('\n');
        moduleOptions.forEach(option -> text.append("module-option=").append(option).append('\n'));
        archiveOptions.forEach(option -> text.append("vm-option=").append(option).append('\n'));
        for (Path jar : jars) {
            BasicFileAttributes attributes = Files.readAttributes(jar, BasicFileAttributes.class);
            text.append("jar=").append(jar).append('|').append(attributes.size()).append('|')
                .append(attributes.lastModifiedTime().toMillis()).append('\n');
        }
        byte[] hash = digest.digest(text.toString().getBytes(StandardCharsets.UTF_8));
        return HexFormat.of().formatHex(hash, 0, KEY_BYTES);
    }

    /**
     * @param path a path inside a JAR or an output directory, with {@code /} separators
     * @return whether the path may exist both in an output and in a JAR: files that are merged or only describe the
     * archive they are in
     */
    static boolean isExempt(String path) {
        return path.equals("META-INF/MANIFEST.MF")
            || path.equals("META-INF/INDEX.LIST")
            || path.equals("module-info.class")
            || path.startsWith("META-INF/services/")
            || path.startsWith("META-INF/micronaut/")
            || path.startsWith("META-INF/versions/")
            || path.startsWith("META-INF/maven/")
            || SIGNATURE_FILE.matcher(path).matches()
            || NOTICE_FILE.matcher(path).matches();
    }

    /**
     * @param jars the JARs, in class path order
     * @return each file path of the JARs that is not exempt, with the index of the first JAR that has it
     * @throws IOException if a JAR cannot be read
     */
    static Map<String, Integer> readEntries(List<Path> jars) throws IOException {
        var index = new HashMap<String, Integer>();
        for (int i = 0; i < jars.size(); i++) {
            try (var zip = new ZipFile(jars.get(i).toFile())) {
                Enumeration<? extends ZipEntry> zipEntries = zip.entries();
                while (zipEntries.hasMoreElements()) {
                    ZipEntry entry = zipEntries.nextElement();
                    if (!entry.isDirectory() && !isExempt(entry.getName())) {
                        index.putIfAbsent(entry.getName(), i);
                    }
                }
            }
        }
        return index;
    }

    /**
     * Looks up every file of the entries that move after the JARs in the entry index of the JARs.
     *
     * @param layout the class path layout
     * @param jarEntries the entry index of the JARs
     * @return the files that a JAR would now hide
     * @throws IOException if an entry cannot be read
     */
    static List<Duplicate> findDuplicates(Layout layout, Map<String, Integer> jarEntries) throws IOException {
        var duplicates = new ArrayList<Duplicate>();
        var moved = new ArrayList<>(layout.outputs());
        moved.addAll(layout.others());
        for (Path entry : moved) {
            for (String path : files(entry)) {
                Integer jar = jarEntries.get(path);
                if (jar != null && !isExempt(path)) {
                    duplicates.add(new Duplicate(path, entry, layout.jars().get(jar)));
                }
            }
        }
        return duplicates;
    }

    /**
     * Copies a recorded class list up to its last complete line: a JVM that was stopped forcibly (on Windows,
     * {@code Process.destroy()} is forcible) may have written half a line.
     *
     * @param recorded the recorded list
     * @param target the class list to write
     * @return whether the list has at least one complete line
     * @throws IOException if a file cannot be read or written
     */
    static boolean trimToLastCompleteLine(Path recorded, Path target) throws IOException {
        if (!Files.isRegularFile(recorded)) {
            return false;
        }
        byte[] content = Files.readAllBytes(recorded);
        int end = content.length;
        while (end > 0 && content[end - 1] != '\n') {
            end--;
        }
        if (end == 0) {
            return false;
        }
        Path temporary = target.resolveSibling(target.getFileName() + TEMPORARY_SUFFIX);
        Files.write(temporary, Arrays.copyOf(content, end));
        Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        return true;
    }

    private List<String> flags(DumpRequest request) {
        String key = request.key();
        Path archive = file(key, ARCHIVE_SUFFIX);
        if (Files.isRegularFile(archive)) {
            debug("launching with %s, which was validated when it was created, so without a probe".formatted(archive));
            return List.of(SHARED_ARCHIVE_FILE + archive, LOG_OFF);
        }
        if (Files.exists(file(key, FAILED_SUFFIX))) {
            debug("the archive for this class path could not be created, see %s: launching without it".formatted(file(key, LOG_SUFFIX)));
            return List.of();
        }
        if (dumpKey != null) {
            debug("a CDS archive is being created: this launch runs without it and records nothing");
            return List.of();
        }
        if (recording != null) {
            debug("the running launch is recording its class list: this launch runs without an archive and records nothing");
            return List.of();
        }
        if (Files.isRegularFile(request.classList())) {
            startDump(request);
            debug("creating the CDS archive from %s: this launch runs without it".formatted(request.classList()));
            return List.of();
        }
        Path recordingFile = file(key, RECORDING_SUFFIX);
        try {
            Files.createDirectories(directory);
            Files.deleteIfExists(recordingFile);
        } catch (IOException e) {
            debug("could not prepare %s (%s): launching without recording".formatted(recordingFile, e.getMessage()));
            return List.of();
        }
        pendingRecording = new Recording(request, recordingFile);
        if (announcedKeys.add(key)) {
            log.info("Class data sharing: this launch records the classes it loads. When it ends, a CDS archive of the dependency JARs is created in %s for the next launches"
                .formatted(directory));
        }
        return List.of("-XX:DumpLoadedClassList=" + recordingFile);
    }

    private void finishEndedRecording() {
        if (recording != null && !recording.process.isAlive()) {
            finishRecording(false);
        }
    }

    private void finishRecording(boolean stoppedByPlugin) {
        Recording ended = recording;
        recording = null;
        int status = stoppedByPlugin ? 0 : ended.process.exitValue();
        try {
            if (!keepsClassList(stoppedByPlugin, status)) {
                debug("the application exited with status %d: discarding the class list it recorded".formatted(status));
                return;
            }
            if (!trimToLastCompleteLine(ended.file, ended.request.classList())) {
                debug("the launch recorded no class: nothing to archive");
                return;
            }
        } catch (IOException e) {
            debug("could not keep the class list: " + e.getMessage());
            return;
        } finally {
            deleteQuietly(ended.file);
        }
        if (!shuttingDown && ended.request.key().equals(currentKey)) {
            startDump(ended.request);
        }
    }

    private void startDump(DumpRequest request) {
        if (dumpKey != null || shuttingDown) {
            return;
        }
        dumpKey = request.key();
        if (executor == null) {
            executor = Executors.newSingleThreadExecutor(runnable -> {
                var thread = new Thread(runnable, "mn-run-class-data-sharing");
                thread.setDaemon(true);
                return thread;
            });
        }
        dump = executor.submit(() -> dump(request));
    }

    private void dump(DumpRequest request) {
        String key = request.key();
        Path logFile = file(key, LOG_SUFFIX);
        Path temporary = directory.resolve(key + ARCHIVE_SUFFIX + "." + ProcessHandle.current().pid() + "-" + System.nanoTime() + TEMPORARY_SUFFIX);
        long start = System.nanoTime();
        try {
            Set<String> rootModules = new TreeSet<>(request.rootModules());
            if (jvmciEnabled(request.archiveOptions(), temporary)) {
                // HotSpot adds this module when JVMCI is enabled (GraalVM), but a dump always runs without JVMCI
                rootModules.add(JVMCI_MODULE);
            }
            var dumpCommand = new ArrayList<String>();
            dumpCommand.add(javaExecutable);
            dumpCommand.add("-Xshare:dump");
            dumpCommand.add("-XX:SharedClassListFile=" + request.classList());
            dumpCommand.add(SHARED_ARCHIVE_FILE + temporary);
            addModulesOption(dumpCommand, rootModules);
            dumpCommand.addAll(request.archiveOptions());
            dumpCommand.add("-cp");
            dumpCommand.add(join(request.jars()));
            int status = run(dumpCommand, logFile, false, DUMP_TIMEOUT, temporary);
            if (status != 0 || !Files.isRegularFile(temporary)) {
                failed(key, "the dump exited with status " + status, logFile);
                return;
            }
            var probe = new ArrayList<String>();
            probe.add(javaExecutable);
            probe.add("-Xshare:on");
            probe.add(SHARED_ARCHIVE_FILE + temporary);
            addModulesOption(probe, rootModules);
            probe.addAll(request.moduleOptions());
            probe.addAll(request.archiveOptions());
            probe.add("-cp");
            probe.add(request.launchClasspath());
            probe.add(VERSION);
            status = run(probe, logFile, true, PROBE_TIMEOUT, temporary);
            if (status != 0) {
                failed(key, "the JVM refused it in an -Xshare:on probe (status %d)".formatted(status), logFile);
                return;
            }
            Path archive = file(key, ARCHIVE_SUFFIX);
            Files.move(temporary, archive, StandardCopyOption.ATOMIC_MOVE);
            log.info(String.format(Locale.ROOT, "Class data sharing: created %s (%.1f MB) in %.1f s. The next launches use it",
                archive, Files.size(archive) / MEGABYTE, (System.nanoTime() - start) / NANOS_PER_SECOND));
            deleteOtherKeys(key);
        } catch (IOException e) {
            failed(key, e.toString(), logFile);
        } catch (InterruptedException _) {
            Thread.currentThread().interrupt();
        } finally {
            deleteQuietly(temporary);
            synchronized (this) {
                dumpKey = null;
                dumpProcess = null;
                dumpTemporary = null;
            }
        }
    }

    // whether the launch JVM, with the options of the launch, enables JVMCI (GraalVM does by default)
    private boolean jvmciEnabled(List<String> archiveOptions, Path temporary) throws IOException, InterruptedException {
        var command = new ArrayList<String>();
        command.add(javaExecutable);
        command.addAll(archiveOptions);
        command.add("-XX:+PrintFlagsFinal");
        command.add(VERSION);
        Path flags = temporary.resolveSibling(temporary.getFileName() + ".flags");
        try {
            return run(command, flags, false, PROBE_TIMEOUT, temporary) == 0
                && Files.readAllLines(flags).stream().anyMatch(line -> ENABLE_JVMCI.matcher(line).find());
        } finally {
            deleteQuietly(flags);
        }
    }

    private int run(List<String> command, Path logFile, boolean append, Duration timeout, Path temporary)
        throws IOException, InterruptedException {
        var builder = new ProcessBuilder(command)
            .directory(directory.toFile())
            .redirectErrorStream(true)
            .redirectOutput(append ? ProcessBuilder.Redirect.appendTo(logFile.toFile()) : ProcessBuilder.Redirect.to(logFile.toFile()));
        ENVIRONMENT_OPTIONS.forEach(builder.environment()::remove);
        Process process;
        synchronized (this) {
            if (shuttingDown) {
                throw new InterruptedException("Maven is shutting down");
            }
            process = builder.start();
            dumpProcess = process;
            dumpTemporary = temporary;
        }
        try {
            if (!process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS)) {
                process.destroyForcibly();
                process.waitFor();
                return -1;
            }
        } catch (InterruptedException e) {
            process.destroyForcibly();
            throw e;
        }
        return process.exitValue();
    }

    private void failed(String key, String reason, Path logFile) {
        synchronized (this) {
            if (shuttingDown) {
                return;
            }
        }
        try {
            Files.writeString(file(key, FAILED_SUFFIX), reason + System.lineSeparator());
        } catch (IOException e) {
            debug("could not record the failure: " + e.getMessage());
        }
        log.info("Class data sharing: could not create the CDS archive of the dependency JARs: %s. Launches run without it until the dependencies, the JDK or the JVM options change. Details: %s"
            .formatted(reason, logFile));
    }

    private void deleteOtherKeys(String key) {
        String current;
        synchronized (this) {
            current = currentKey;
        }
        try (Stream<Path> files = Files.list(directory)) {
            files.filter(file -> {
                String name = file.getFileName().toString();
                int dot = name.indexOf('.');
                String fileKey = dot < 0 ? name : name.substring(0, dot);
                return !fileKey.equals(key) && !fileKey.equals(current);
            }).forEach(ClassDataSharingSupport::deleteQuietly);
        } catch (IOException e) {
            debug("could not delete older archives: " + e.getMessage());
        }
    }

    private void deleteStaleTemporaries() throws IOException {
        if (staleTemporariesDeleted || !Files.isDirectory(directory)) {
            return;
        }
        staleTemporariesDeleted = true;
        Instant limit = Instant.now().minus(STALE_TEMPORARY);
        try (Stream<Path> files = Files.list(directory)) {
            files.filter(file -> file.getFileName().toString().endsWith(TEMPORARY_SUFFIX))
                .filter(file -> {
                    try {
                        FileTime modified = Files.getLastModifiedTime(file);
                        return modified.toInstant().isBefore(limit);
                    } catch (IOException _) {
                        return false;
                    }
                })
                .forEach(ClassDataSharingSupport::deleteQuietly);
        }
    }

    private Map<String, Integer> entries(String key, List<Path> jars) throws IOException {
        if (key.equals(entriesKey)) {
            return entries;
        }
        Path file = file(key, ENTRIES_SUFFIX);
        Map<String, Integer> index = Files.isRegularFile(file) ? readIndex(file, jars.size()).orElse(null) : null;
        if (index == null) {
            index = readEntries(jars);
            Files.createDirectories(directory);
            Path temporary = file.resolveSibling(file.getFileName() + TEMPORARY_SUFFIX);
            Files.write(temporary, index.entrySet().stream()
                .map(entry -> entry.getValue() + "\t" + entry.getKey())
                .sorted()
                .toList(), StandardCharsets.UTF_8);
            Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        }
        entriesKey = key;
        entries = index;
        return index;
    }

    private static Optional<Map<String, Integer>> readIndex(Path file, int jarCount) throws IOException {
        var index = new HashMap<String, Integer>();
        for (String line : Files.readAllLines(file, StandardCharsets.UTF_8)) {
            int tab = line.indexOf('\t');
            try {
                int jar = Integer.parseInt(line.substring(0, Math.max(tab, 0)));
                if (tab < 0 || jar < 0 || jar >= jarCount) {
                    return Optional.empty();
                }
                index.put(line.substring(tab + 1), jar);
            } catch (NumberFormatException _) {
                return Optional.empty();
            }
        }
        return Optional.of(index);
    }

    private void warnAboutDuplicates(List<Duplicate> duplicates) {
        String listed = duplicates.stream()
            .limit(MAX_REPORTED_DUPLICATES)
            .map(duplicate -> "%s (in %s and in %s)".formatted(duplicate.path(), duplicate.entry(), duplicate.jar()))
            .collect(Collectors.joining(", "));
        if (duplicates.size() > MAX_REPORTED_DUPLICATES) {
            listed += " and %d more".formatted(duplicates.size() - MAX_REPORTED_DUPLICATES);
        }
        String warning = ("Class data sharing is off for this launch: %s. Putting the dependency JARs first would change "
            + "which copy the application sees, so mn:run keeps the usual class path order and runs without a CDS archive").formatted(listed);
        if (warning.equals(lastDuplicateWarning)) {
            debug(warning);
        } else {
            log.warn(warning);
            lastDuplicateWarning = warning;
        }
    }

    private Optional<Jdk> jdk() {
        if (!jdkRead) {
            jdkRead = true;
            jdk = readJdk().orElse(null);
            if (jdk == null) {
                log.info("Class data sharing: could not read the version of %s, so mn:run launches the application without a CDS archive"
                    .formatted(javaExecutable));
            }
        }
        return Optional.ofNullable(jdk);
    }

    private Optional<Jdk> readJdk() {
        Path output = null;
        try {
            Files.createDirectories(directory);
            output = directory.resolve("java-properties-" + ProcessHandle.current().pid() + TEMPORARY_SUFFIX);
            var builder = new ProcessBuilder(javaExecutable, "-XshowSettings:properties", VERSION)
                .redirectErrorStream(true)
                .redirectOutput(output.toFile());
            ENVIRONMENT_OPTIONS.forEach(builder.environment()::remove);
            Process process = builder.start();
            if (!process.waitFor(PROBE_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS)) {
                process.destroyForcibly();
                return Optional.empty();
            }
            if (process.exitValue() != 0) {
                return Optional.empty();
            }
            return Jdk.parse(Files.readAllLines(output));
        } catch (IOException e) {
            debug("could not run %s: %s".formatted(javaExecutable, e.getMessage()));
            return Optional.empty();
        } catch (InterruptedException _) {
            Thread.currentThread().interrupt();
            return Optional.empty();
        } finally {
            deleteQuietly(output);
        }
    }

    private Path file(String key, String suffix) {
        return directory.resolve(key + suffix);
    }

    private void debug(String message) {
        if (log.isDebugEnabled()) {
            log.debug("Class data sharing: " + message);
        }
    }

    private static List<String> environmentOptions(Map<String, String> environment, String variable) {
        String value = environment.get(variable);
        if (value == null || value.isBlank()) {
            return List.of();
        }
        try {
            return Arrays.asList(CommandLineUtils.translateCommandline(value));
        } catch (Exception _) {
            return Arrays.asList(value.trim().split("\\s+"));
        }
    }

    private static boolean managesCdsOrModuleGraph(String option) {
        if (option.startsWith("-Xshare") || option.startsWith("@")) {
            return true;
        }
        String flag = flagName(option);
        if (flag != null) {
            return CDS_FLAGS.contains(flag) || OPTION_FILE_FLAGS.contains(flag);
        }
        return MODULE_GRAPH_OPTIONS.stream().anyMatch(graphOption -> option.equals(graphOption) || option.startsWith(graphOption + "="));
    }

    private static String flagName(String option) {
        if (!option.startsWith("-XX:")) {
            return null;
        }
        String flag = option.substring("-XX:".length());
        if (flag.startsWith("+") || flag.startsWith("-")) {
            flag = flag.substring(1);
        }
        // the JFR options also take their value after a colon, as in -XX:StartFlightRecording:filename=recording.jfr
        int end = 0;
        while (end < flag.length() && flag.charAt(end) != '=' && flag.charAt(end) != ':') {
            end++;
        }
        return flag.substring(0, end);
    }

    private static void addModulesOption(List<String> command, Set<String> rootModules) {
        if (!rootModules.isEmpty()) {
            command.add(ADD_MODULES + "=" + String.join(",", rootModules));
        }
    }

    private static boolean isJar(Path path) {
        Path name = path.getFileName();
        return name != null && name.toString().toLowerCase(Locale.ROOT).endsWith(".jar") && Files.isRegularFile(path);
    }

    private static String join(Collection<Path> paths) {
        return paths.stream().map(Path::toString).collect(Collectors.joining(File.pathSeparator));
    }

    private static List<String> files(Path entry) throws IOException {
        if (Files.isDirectory(entry)) {
            try (Stream<Path> files = Files.walk(entry)) {
                return files.filter(Files::isRegularFile)
                    .map(file -> entry.relativize(file).toString().replace(File.separatorChar, '/'))
                    .toList();
            }
        }
        if (Files.isRegularFile(entry)) {
            try (var zip = new ZipFile(entry.toFile())) {
                return zip.stream().filter(zipEntry -> !zipEntry.isDirectory()).map(ZipEntry::getName).toList();
            } catch (IOException _) {
                return List.of();
            }
        }
        return List.of();
    }

    private static void deleteQuietly(Path file) {
        if (file != null) {
            try {
                Files.deleteIfExists(file);
            } catch (IOException _) {
                // a running JVM may still map it (Windows refuses to delete a mapped file)
            }
        }
    }

    /**
     * The JDK of the launch.
     *
     * @param home its {@code java.home}
     * @param vmVersion its {@code java.vm.version}, which names the build
     * @param feature its feature version
     */
    record Jdk(String home, String vmVersion, int feature) {

        static Optional<Jdk> parse(List<String> showSettingsOutput) {
            String home = null;
            String vmVersion = null;
            String specification = null;
            for (String line : showSettingsOutput) {
                String trimmed = line.trim();
                int equals = trimmed.indexOf(" = ");
                if (equals > 0) {
                    String name = trimmed.substring(0, equals);
                    String value = trimmed.substring(equals + " = ".length());
                    switch (name) {
                        case "java.home" -> home = value;
                        case "java.vm.version" -> vmVersion = value;
                        case "java.specification.version" -> specification = value;
                        default -> {
                            // not needed
                        }
                    }
                }
            }
            if (home == null || vmVersion == null || specification == null) {
                return Optional.empty();
            }
            try {
                String major = specification.startsWith("1.") ? specification.substring(2) : specification;
                int dot = major.indexOf('.');
                return Optional.of(new Jdk(home, vmVersion, Integer.parseInt(dot < 0 ? major : major.substring(0, dot))));
            } catch (NumberFormatException _) {
                return Optional.empty();
            }
        }
    }

    /**
     * The class path of a launch with this option: the JARs, then the reactor outputs, then the other entries.
     *
     * @param jars the regular JAR files of the resolved class path, in its order
     * @param outputs the reactor output directories
     * @param others the dependency entries that are not regular JAR files
     */
    record Layout(List<Path> jars, List<Path> outputs, List<Path> others) {

        String classpath() {
            var all = new ArrayList<>(jars);
            all.addAll(outputs);
            all.addAll(others);
            return join(all);
        }
    }

    /**
     * A file of an entry that moves after the JARs, which a JAR also has.
     *
     * @param path the path of the file, with {@code /} separators
     * @param entry the class path entry that has it
     * @param jar the first JAR that has it
     */
    record Duplicate(String path, Path entry, Path jar) {
    }

    /**
     * What a dump needs.
     *
     * @param key the key of the archive
     * @param classList the class list
     * @param jars the JARs to archive
     * @param rootModules the root modules of the launch
     * @param moduleOptions the other module options of the launch
     * @param archiveOptions the {@code -XX:} and heap options of the launch
     * @param launchClasspath the class path of the launch, for the probe
     */
    record DumpRequest(String key, Path classList, List<Path> jars, Set<String> rootModules, List<String> moduleOptions,
                       List<String> archiveOptions, String launchClasspath) {
    }

    /**
     * A launch that records its class list.
     */
    private static final class Recording {

        private final DumpRequest request;
        private final Path file;
        private Process process;

        private Recording(DumpRequest request, Path file) {
            this.request = request;
            this.file = file;
        }
    }
}
