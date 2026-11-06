package io.micronaut.maven.logback;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.zip.CRC32;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class PrecompileLogbackMojoTest {

    @TempDir
    Path target;

    @Test
    void removesOnlyUnchangedEarlierOutputWhenDisabled() throws Exception {
        Path classes = Files.createDirectories(target.resolve("classes"));
        Path configurator = classes.resolve("io/micronaut/aot/logback/generated/LogbackConfigurator.class");
        Path service = classes.resolve("META-INF/services/ch.qos.logback.classic.spi.Configurator");
        Files.createDirectories(configurator.getParent());
        Files.createDirectories(service.getParent());
        byte[] generated = {1, 2, 3};
        Files.write(configurator, generated);
        Files.writeString(service, "io.micronaut.aot.logback.generated.LogbackConfigurator\n");
        Path record = target.resolve(PrecompileLogbackMojo.RECORD_FILE);
        Files.createDirectories(record.getParent());
        Files.write(record, List.of(
            "io/micronaut/aot/logback/generated/LogbackConfigurator.class\t3\t" + crc(generated),
            "META-INF/services/ch.qos.logback.classic.spi.Configurator\t4\t0"
        ), StandardCharsets.UTF_8);

        MavenProject project = mock(MavenProject.class);
        Build build = new Build();
        build.setDirectory(target.toString());
        build.setOutputDirectory(classes.toString());
        when(project.getBuild()).thenReturn(build);

        new PrecompileLogbackMojo(project).execute();

        assertFalse(Files.exists(configurator));
        // Rewritten since it was recorded, as aot-analysis does: it stays
        assertTrue(Files.exists(service));
        assertFalse(Files.exists(record));
    }

    @Test
    void precompilesLogbackXmlAndRemovesItsOutputOnceTurnedOff() throws Exception {
        Path classes = classesWithLogbackXml();
        var project = project(classes, "25", logbackArtifacts());
        var log = mock(Log.class);
        var mojo = mojo(project, true, log);

        mojo.execute();

        Path record = target.resolve(PrecompileLogbackMojo.RECORD_FILE);
        List<String> written = Files.readAllLines(record).stream().map(line -> line.split("\t")[0]).toList();
        assertFalse(written.isEmpty());
        for (String entry : written) {
            assertTrue(Files.isRegularFile(classes.resolve(entry)), entry);
        }
        assertTrue(Files.exists(classes.resolve("logback.xml")));
        verify(log, never()).warn(any(CharSequence.class));

        mojo(project, false, mock(Log.class)).execute();

        for (String entry : written) {
            assertFalse(Files.exists(classes.resolve(entry)), entry);
        }
        assertFalse(Files.exists(record));
    }

    @Test
    void readsTheTargetReleaseFromMavenCompilerTarget() throws Exception {
        Path classes = classesWithLogbackXml();
        var project = project(classes, null, logbackArtifacts());
        project.getProperties().setProperty("maven.compiler.release", "not-a-number");
        project.getProperties().setProperty("maven.compiler.target", "1.8");
        var log = mock(Log.class);

        mojo(project, true, log).execute();

        verify(log).debug("Ignoring maven.compiler.release=not-a-number");
        verify(log, never()).info("No Logback configuration was precompiled because the target Java release is "
            + "unknown: set maven.compiler.release");
    }

    @Test
    void skipsWithoutATargetRelease() throws Exception {
        Path classes = classesWithLogbackXml();
        var log = mock(Log.class);

        mojo(project(classes, null, logbackArtifacts()), true, log).execute();

        verify(log).info("No Logback configuration was precompiled because the target Java release is unknown: "
            + "set maven.compiler.release");
        assertFalse(Files.exists(target.resolve(PrecompileLogbackMojo.RECORD_FILE)));
    }

    @Test
    void skipsWithoutAnOutputDirectory() throws Exception {
        var log = mock(Log.class);

        mojo(project(target.resolve("missing"), "25", List.of()), true, log).execute();

        verifyNoInteractions(log);
    }

    @Test
    void writesNothingWithoutLogbackXml() throws Exception {
        Path classes = Files.createDirectories(target.resolve("classes"));
        var log = mock(Log.class);

        mojo(project(classes, "25", logbackArtifacts()), true, log).execute();

        assertFalse(Files.exists(target.resolve(PrecompileLogbackMojo.RECORD_FILE)));
        try (var files = Files.list(classes)) {
            assertEquals(0, files.count());
        }
    }

    @Test
    void leavesAConfigurationThatLogbackCannotReadToLogback() throws Exception {
        Path classes = Files.createDirectories(target.resolve("classes"));
        Files.writeString(classes.resolve("logback.xml"), "<configuration><appender");
        var log = mock(Log.class);

        mojo(project(classes, "25", logbackArtifacts()), true, log).execute();

        assertFalse(Files.exists(target.resolve(PrecompileLogbackMojo.RECORD_FILE)));
        var message = ArgumentCaptor.forClass(CharSequence.class);
        verify(log).info(message.capture());
        assertTrue(message.getValue().toString().startsWith("No Logback configuration was precompiled because "
            + "logback.xml (application output) is outside what the precompiler can reproduce exactly"), message.getValue().toString());
        verify(log, never()).warn(any(CharSequence.class));
    }

    @Test
    void warnsWhenTheConfigurationCannotBeCompiled() throws Exception {
        Path classes = classesWithLogbackXml();
        // An SLF4J without the interface that Logback implements, as SLF4J 1.7 is
        var artifacts = new ArrayList<Artifact>();
        for (Artifact artifact : logbackArtifacts()) {
            if (artifact.getFile().getName().startsWith("slf4j-api")) {
                artifacts.add(artifact(withoutEntry(artifact.getFile(), "org/slf4j/spi/LoggingEventAware.class"),
                    Artifact.SCOPE_COMPILE));
            } else {
                artifacts.add(artifact);
            }
        }
        var log = mock(Log.class);

        mojo(project(classes, "25", artifacts), true, log).execute();

        assertFalse(Files.exists(target.resolve(PrecompileLogbackMojo.RECORD_FILE)));
        var message = ArgumentCaptor.forClass(CharSequence.class);
        verify(log).warn(message.capture());
        assertTrue(message.getValue().toString().startsWith("No Logback configuration was precompiled because it could "
            + "not be compiled"), message.getValue().toString());
    }

    private Path classesWithLogbackXml() throws Exception {
        Path classes = Files.createDirectories(target.resolve("classes"));
        Files.writeString(classes.resolve("logback.xml"), """
            <configuration>
                <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
                    <encoder>
                        <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>
                    </encoder>
                </appender>
                <root level="info">
                    <appender-ref ref="STDOUT"/>
                </root>
            </configuration>
            """);
        return classes;
    }

    private MavenProject project(Path classes, String release, List<Artifact> artifacts) {
        MavenProject project = mock(MavenProject.class);
        Build build = new Build();
        build.setDirectory(target.toString());
        build.setOutputDirectory(classes.toString());
        when(project.getBuild()).thenReturn(build);
        var properties = new Properties();
        if (release != null) {
            properties.setProperty("maven.compiler.release", release);
        }
        when(project.getProperties()).thenReturn(properties);
        when(project.getArtifacts()).thenReturn(new LinkedHashSet<>(artifacts));
        return project;
    }

    private static PrecompileLogbackMojo mojo(MavenProject project, boolean precompile, Log log) throws Exception {
        var mojo = new PrecompileLogbackMojo(project);
        Field field = PrecompileLogbackMojo.class.getDeclaredField("precompile");
        field.setAccessible(true);
        field.setBoolean(mojo, precompile);
        mojo.setLog(log);
        return mojo;
    }

    /**
     * @return Logback and SLF4J as compile and runtime dependencies, and a test dependency that stays off the class path
     */
    private static List<Artifact> logbackArtifacts() throws Exception {
        var artifacts = new ArrayList<Artifact>();
        for (Class<?> type : List.of(ch.qos.logback.classic.LoggerContext.class, ch.qos.logback.core.Context.class,
            org.slf4j.Logger.class)) {
            File jar = new File(type.getProtectionDomain().getCodeSource().getLocation().toURI());
            artifacts.add(artifact(jar, artifacts.isEmpty() ? Artifact.SCOPE_RUNTIME : Artifact.SCOPE_COMPILE));
        }
        artifacts.add(artifact(new File("missing.jar"), Artifact.SCOPE_COMPILE));
        artifacts.add(artifact(new File(Test.class.getProtectionDomain().getCodeSource().getLocation().toURI()),
            Artifact.SCOPE_TEST));
        return artifacts;
    }

    private File withoutEntry(File jar, String excluded) throws Exception {
        File copy = target.resolve("lib/" + jar.getName()).toFile();
        Files.createDirectories(copy.toPath().getParent());
        try (var in = new JarFile(jar); var out = new JarOutputStream(Files.newOutputStream(copy.toPath()))) {
            for (JarEntry entry : Collections.list(in.entries())) {
                if (!entry.getName().equals(excluded)) {
                    out.putNextEntry(new JarEntry(entry.getName()));
                    try (var content = in.getInputStream(entry)) {
                        content.transferTo(out);
                    }
                    out.closeEntry();
                }
            }
        }
        return copy;
    }

    private static Artifact artifact(File file, String scope) {
        var handler = mock(ArtifactHandler.class);
        when(handler.isAddedToClasspath()).thenReturn(true);
        var artifact = mock(Artifact.class);
        when(artifact.getFile()).thenReturn(file);
        when(artifact.getScope()).thenReturn(scope);
        when(artifact.getArtifactHandler()).thenReturn(handler);
        return artifact;
    }

    private static long crc(byte[] content) {
        var crc = new CRC32();
        crc.update(content);
        return crc.getValue();
    }
}
