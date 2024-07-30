package io.micronaut.maven;

import io.methvin.watcher.DirectoryChangeEvent;
import io.methvin.watcher.hashing.FileHash;
import io.micronaut.maven.services.CompilerService;
import io.micronaut.maven.services.DependencyResolutionService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.FileSet;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.toolchain.ToolchainManager;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static io.micronaut.maven.RunMojo.THIS_PLUGIN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class FileWatchingTest {

    @TempDir
    private Path tempDir;

    @Test
    @DisplayName("should detect changes in a single module project")
    void testFileWatchesSingleProject() throws IOException, MojoExecutionException {
        // given:
        var mojo = createMojoForSingleProject();
        var javaSources = tempDir.resolve("src/main/java");
        var javaResources = tempDir.resolve("src/main/resources");

        // when:
        mojo.withChange(createFile(javaSources.resolve("Dummy.java"), "public class Dummy {}"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(updateFile(javaSources.resolve("Dummy.java"), "public class Dummy { public static void main(String[] args) {} }"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaSources.resolve("file.back~"), "excludes some files by default"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(tempDir.resolve(".ignored/file"), "should not trigger recompilation"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(tempDir.resolve("pom.xml"), "should detect changes to pom.xml"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaResources.resolve("application.yml"), "should detect changes to application configuration"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaResources.resolve("META-INF/version.txt"), "detects changes to resources directory"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.addWatch(new FileSet() {{
            setDirectory(javaResources.resolve("META-INF").toString());
            setExcludes(List.of("**/*"));
        }});
        mojo.withChange(createFile(javaResources.resolve("META-INF/version.txt"), "changes to META-INF are now explicitly ignored"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        List.of("groovy", "kotlin").forEach(lang -> {
            mojo.withChange(createFile(tempDir.resolve("src/main/" + lang + "/Dummy." + lang), "public class Dummy { public static void main(String[] args) {} }"));
            // then:
            mojo.assertRecompiled();
        });

        // when:
        mojo.withChange(createFile(tempDir.resolve("src/main/scala/Dummy.java"), "I'm not a supported language"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(tempDir.resolve("unrelated.file"), "I'm not a source file"));
        // then:
        mojo.assertNotRecompiled();
    }

    @Test
    @DisplayName("should detect changes in a multi-module project")
    void testFileWatchesMultiProject() throws IOException, MojoExecutionException {
        // given:
        var mojo = createMojoForMultiProject(2, 0);
        var module0 = tempDir.resolve("module0");
        var module1 = tempDir.resolve("module1");
        var javaSources = module0.resolve("src/main/java");
        var javaResources = module0.resolve("src/main/resources");

        // when:
        mojo.withChange(createFile(javaSources.resolve("Dummy.java"), "public class Dummy {}"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(updateFile(javaSources.resolve("Dummy.java"), "public class Dummy { public static void main(String[] args) {} }"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaSources.resolve("file.back~"), "excludes some files by default"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(module0.resolve(".ignored/file"), "should not trigger recompilation"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(module0.resolve("pom.xml"), "should detect changes to pom.xml"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaResources.resolve("application.yml"), "should detect changes to application configuration"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(javaResources.resolve("META-INF/version.txt"), "detects changes to resources directory"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.addWatch(new FileSet() {{
            setDirectory(javaResources.resolve("META-INF").toString());
            setExcludes(List.of("**/*"));
        }});
        mojo.withChange(createFile(javaResources.resolve("META-INF/version.txt"), "changes to META-INF are now explicitly ignored"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        List.of("groovy", "kotlin").forEach(lang -> {
            mojo.withChange(createFile(module0.resolve("src/main/" + lang + "/Dummy." + lang), "public class Dummy { public static void main(String[] args) {} }"));
            // then:
            mojo.assertRecompiled();
        });

        // when:
        mojo.withChange(createFile(module0.resolve("src/main/scala/Dummy.java"), "I'm not a supported language"));
        // then:
        mojo.assertNotRecompiled();

        // when:
        mojo.withChange(createFile(module1.resolve("src/main/java/Dummy.java"), "Detects change in another module"));
        // then:
        mojo.assertRecompiled();

        // when:
        mojo.withChange(createFile(module1.resolve(".toto/ignored"), "Ignores change in another module"));
        // then:
        mojo.assertNotRecompiled();

    }

    private MojoUnderTest createMojoForSingleProject() throws MojoExecutionException {
        var project = newProject(tempDir);
        var compilerService = createCompilerServices();
        var mavenSession = mock(MavenSession.class);
        when(mavenSession.getTopLevelProject()).thenReturn(project);
        when(mavenSession.getProjects()).thenReturn(List.of(project));
        when(mavenSession.getAllProjects()).thenReturn(List.of(project));
        when(mavenSession.getCurrentProject()).thenReturn(project);
        var thisPlugin = new Plugin();
        thisPlugin.setGroupId("io.micronaut.maven");
        thisPlugin.setArtifactId("micronaut-maven-plugin");
        when(project.getPlugin(THIS_PLUGIN)).thenReturn(thisPlugin);
        when(project.getBuildPlugins()).thenReturn(List.of(thisPlugin));
        return createMojoUnderTest(mavenSession, compilerService);
    }

    @NotNull
    private static CompilerService createCompilerServices() {
        var compilerService = mock(CompilerService.class);
        when(compilerService.compileProject()).thenReturn(Optional.of(123L));
        return compilerService;
    }

    @NotNull
    private MavenProject newProject(Path baseDir) {
        var build = mock(Build.class);
        when(build.getDirectory()).thenReturn("target");
        var project = mock(MavenProject.class);
        when(project.getGroupId()).thenReturn("io.micronaut.maven");
        when(project.getArtifactId()).thenReturn(baseDir.getFileName().toString());
        when(project.getVersion()).thenReturn("1.0.0");
        when(project.getBuild()).thenReturn(build);
        when(project.getBasedir()).thenReturn(baseDir.toFile());
        return project;
    }

    private MojoUnderTest createMojoForMultiProject(int moduleCount, int consideredProject) throws MojoExecutionException {
        var rootProject = newProject(tempDir);
        var mavenSession = mock(MavenSession.class);
        when(mavenSession.getTopLevelProject()).thenReturn(rootProject);
        var modules = IntStream.range(0, moduleCount)
            .mapToObj(i -> "module" + i)
            .map(tempDir::resolve)
            .map(this::newProject)
            .toList();
        var compilerService = createCompilerServices();
        var runnableProject = modules.get(consideredProject);
        var thisPlugin = new Plugin();
        thisPlugin.setGroupId("io.micronaut.maven");
        thisPlugin.setArtifactId("micronaut-maven-plugin");
        when(runnableProject.getPlugin(THIS_PLUGIN)).thenReturn(thisPlugin);
        when(runnableProject.getBuildPlugins()).thenReturn(List.of(thisPlugin));
        var dependencies = modules.stream()
            .filter(p -> !p.equals(runnableProject))
            .filter(p -> !p.equals(rootProject))
            .map(p -> {
                var dependency = new Dependency();
                dependency.setGroupId(p.getGroupId());
                dependency.setArtifactId(p.getArtifactId());
                dependency.setVersion(p.getVersion());
                return dependency;
            })
            .toList();
        when(runnableProject.getDependencies()).thenReturn(dependencies);
        when(mavenSession.getProjects()).thenReturn(modules);
        when(mavenSession.getCurrentProject()).thenReturn(runnableProject);
        when(mavenSession.getAllProjects()).thenReturn(
            Stream.concat(Stream.of(rootProject), modules.stream()).toList()
        );
        return createMojoUnderTest(mavenSession, compilerService);
    }

    private static MojoUnderTest createMojoUnderTest(MavenSession mavenSession, CompilerService compilerService) throws MojoExecutionException {
        var recompilationCount = new AtomicInteger();
        var mojo = new RunMojo(
            mavenSession,
            mock(BuildPluginManager.class),
            mock(ProjectBuilder.class),
            mock(ToolchainManager.class),
            compilerService,
            mock(ExecutorService.class),
            mock(DependencyResolutionService.class)
        ) {
            @Override
            protected void runApplication() throws Exception {
                recompilationCount.incrementAndGet();
            }
        };
        var watches = new ArrayList<FileSet>();
        mojo.setWatches(watches);
        mojo.initialize();
        return new MojoUnderTest(mojo, recompilationCount, watches);
    }

    private static DirectoryChangeEvent createFile(Path file, String content) {
        try {
            Files.createDirectories(file.getParent());
            Files.writeString(file, content);
            return new DirectoryChangeEvent(
                DirectoryChangeEvent.EventType.CREATE,
                true,
                file,
                mock(FileHash.class),
                0,
                null
            );
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static DirectoryChangeEvent updateFile(Path file, String content) throws IOException {
        Files.writeString(file, content);
        return new DirectoryChangeEvent(
            DirectoryChangeEvent.EventType.MODIFY,
            true,
            file,
            mock(FileHash.class),
            0,
            null
        );
    }

    private static class MojoUnderTest {
        private final RunMojo mojo;
        private final AtomicInteger recompilationCount;
        private final List<FileSet> watches;
        private int lastRecompilationCount;

        private MojoUnderTest(RunMojo mojo, AtomicInteger recompilationCount, List<FileSet> watches) {
            this.mojo = mojo;
            this.recompilationCount = recompilationCount;
            this.watches = watches;
        }

        public void withChange(DirectoryChangeEvent event) {
            mojo.handleEvent(event);
        }

        public void addWatch(FileSet watch) {
            watches.add(watch);
        }

        public void assertRecompiled() {
            assertEquals(lastRecompilationCount + 1, recompilationCount.get());
            lastRecompilationCount = recompilationCount.get();
        }

        public void assertNotRecompiled() {
            assertEquals(lastRecompilationCount, recompilationCount.get());
        }
    }
}
