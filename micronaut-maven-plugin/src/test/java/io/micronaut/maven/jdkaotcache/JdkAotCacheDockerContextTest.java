package io.micronaut.maven.jdkaotcache;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JdkAotCacheDockerContextTest {

    @Test
    void writesTheApplicationJarTheClassPathAndTheScript(@TempDir Path tempDir) throws IOException {
        Path classes = classes(tempDir);
        Path target = tempDir.resolve("target");

        JdkAotCacheDockerContext.write(target, classes, List.of("/home/app/libs/release/a.jar", "/home/app/libs/snapshot/b-1.0-SNAPSHOT.jar"));

        Path context = target.resolve("jdk-aot-cache");
        assertEquals("\"/home/app/libs/release/a.jar:/home/app/libs/snapshot/b-1.0-SNAPSHOT.jar:/home/app/application.jar\"\n",
            Files.readString(context.resolve("classpath")));
        assertEquals(JdkAotCacheTraining.readScript(), Files.readString(context.resolve("training.sh")));
        try (var jar = new JarFile(context.resolve("application.jar").toFile())) {
            assertEquals(List.of("META-INF/", "META-INF/MANIFEST.MF", "META-INF/micronaut/", "META-INF/micronaut/service",
                "application.yml", "example/", "example/Application.class"), jar.stream().map(ZipEntry::getName).toList());
            assertEquals("1.0", jar.getManifest().getMainAttributes().getValue("Manifest-Version"));
            assertTrue(jar.stream().allMatch(entry -> LocalDateTime.of(1980, 2, 1, 0, 0).equals(entry.getTimeLocal())));
            assertEquals("class", new String(jar.getInputStream(jar.getEntry("example/Application.class")).readAllBytes()));
        }
    }

    @Test
    void theJarIsTheSameForTheSameClasses(@TempDir Path tempDir) throws IOException {
        Path classes = classes(tempDir);
        Path first = tempDir.resolve("first.jar");
        Path second = tempDir.resolve("second.jar");

        JdkAotCacheDockerContext.writeApplicationJar(classes, first);
        JdkAotCacheDockerContext.writeApplicationJar(classes, second);

        assertArrayEquals(Files.readAllBytes(first), Files.readAllBytes(second));
    }

    @Test
    void writesAnEmptyJarWithoutClasses(@TempDir Path tempDir) throws IOException {
        Path jarPath = tempDir.resolve("application.jar");

        JdkAotCacheDockerContext.writeApplicationJar(tempDir.resolve("missing"), jarPath);

        try (var jar = new JarFile(jarPath.toFile())) {
            assertEquals(List.of("META-INF/", "META-INF/MANIFEST.MF"), jar.stream().map(ZipEntry::getName).toList());
        }
    }

    private static Path classes(Path tempDir) throws IOException {
        Path classes = tempDir.resolve("classes");
        Files.createDirectories(classes.resolve("example"));
        Files.createDirectories(classes.resolve("META-INF/micronaut"));
        Files.writeString(classes.resolve("example/Application.class"), "class");
        Files.writeString(classes.resolve("application.yml"), "micronaut: {}");
        Files.writeString(classes.resolve("META-INF/micronaut/service"), "example.Application");
        Files.writeString(classes.resolve("META-INF/MANIFEST.MF"), "Manifest-Version: 2.0\n");
        return classes;
    }
}
