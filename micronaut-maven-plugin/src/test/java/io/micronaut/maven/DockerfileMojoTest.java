package io.micronaut.maven;

import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import io.micronaut.maven.services.ExecutorService;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class DockerfileMojoTest {

    @Test
    void processDockerfileEscapesJsonAndShellContexts(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080 8443/tcp"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App$USER `touch /tmp/pwned` \"Quoted\" \\\\";
        mojo.baseImageRun = "cgr.dev/chainguard/wolfi-base:latest";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "FROM ${BASE_IMAGE}",
            "EXPOSE ${PORTS}",
            "ENTRYPOINT [\"java\", \"-cp\", \"/app\", \"${CLASS_NAME}\"]",
            "RUN echo ${CLASS_NAME}",
            "RUN native-image -H:Class=\"${CLASS_NAME}\""
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        var jsonClassName = AbstractDockerMojo.escapeJsonString("exec.mainClass", mojo.mainClass);
        var shellLiteralClassName = AbstractDockerMojo.shellLiteral("exec.mainClass", mojo.mainClass);
        var doubleQuotedClassName = mojo.mainClass
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("$", "\\$")
            .replace("`", "\\`");

        assertEquals(
            java.util.List.of(
                "FROM ghcr.io/example/builder:1.0",
                "EXPOSE 8080 8443/tcp",
                "ENTRYPOINT [\"java\", \"-cp\", \"/app\", \"" + jsonClassName + "\"]",
                "RUN echo " + shellLiteralClassName,
                "RUN native-image -H:Class=\"" + doubleQuotedClassName + "\""
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DockerfileNative",
        "DockerfileNativeDistroless",
        "DockerfileNativeStatic",
        "DockerfileNativeLambda"
    })
    void nativeDockerfileTemplatesQuoteClassNameExpansion(String dockerfileName) throws IOException {
        var dockerfile = Path.of("src", "main", "resources", "dockerfiles", dockerfileName);
        var content = Files.readString(dockerfile);

        assertEquals(1, content.lines().filter(line -> line.contains("-H:Class=\"${CLASS_NAME}\"")).count());
        assertFalse(content.contains("-H:Class=${CLASS_NAME}"));
    }

    @Test
    void processDockerfileRejectsInjectedMainClass(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App\"\nRUN echo injected";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "ENTRYPOINT [\"java\", \"${CLASS_NAME}\"]");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("exec.mainClass contains an unsupported control character"));
    }

    @Test
    void processDockerfileRejectsInjectedMainClassInDoubleQuotedNativeImageBranch(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.mainClass = "example.App\nRUN echo injected";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "RUN native-image -H:Class=\"${CLASS_NAME}\"");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("exec.mainClass contains an unsupported control character"));
    }

    @Test
    void processDockerfileRejectsInvalidBaseImageReference(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";
        mojo.baseImageRun = "invalid image";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "FROM ${BASE_IMAGE_RUN}");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("micronaut.native-image.base-image-run is not a valid Docker image reference"));
    }

    @Test
    void processDockerfileRejectsInvalidPortTokens(@TempDir Path tempDir) throws IOException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080 RUN"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "EXPOSE ${PORTS}");

        var exception = assertThrows(MojoExecutionException.class, () -> invokeProcessDockerfile(mojo, dockerfile));

        assertTrue(exception.getMessage().contains("jib.container.ports contains an invalid exposed port token"));
    }

    @Test
    void processDockerfileQuotesDownloadUrlInRunInstruction(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        project.getProperties().setProperty("maven.compiler.release", "25");
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), "RUN curl -4 -L ${GRAALVM_DOWNLOAD_URL} -o /tmp/graalvm.tar.gz");

        invokeProcessDockerfile(mojo, dockerfile);

        assertTrue(Files.readString(dockerfile).contains("-L " + AbstractDockerMojo.shellLiteral("GraalVM download URL", mojo.graalVmDownloadUrl()) + " -o"));
    }

    @Test
    void processDockerfilePreservesUnknownArgLines(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "ARG BASE_IMAGE",
            "ARG CHECKPOINT_IMAGE",
            "ARG CRAC_ARCH",
            "FROM ${BASE_IMAGE}",
            "FROM ${CHECKPOINT_IMAGE} AS crac-checkpoint",
            "RUN echo $CRAC_ARCH"
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            java.util.List.of(
                "ARG CHECKPOINT_IMAGE",
                "ARG CRAC_ARCH",
                "FROM ghcr.io/example/builder:1.0",
                "FROM ${CHECKPOINT_IMAGE} AS crac-checkpoint",
                "RUN echo $CRAC_ARCH"
            ),
            Files.readAllLines(dockerfile)
        );
    }

    @Test
    void processDockerfilePreservesSimilarlyNamedArgLines(@TempDir Path tempDir) throws IOException, MojoExecutionException {
        var project = mockProject(tempDir);
        var jibConfigurationService = mock(JibConfigurationService.class);
        when(jibConfigurationService.getFromImage()).thenReturn(Optional.of("ghcr.io/example/builder:1.0"));
        when(jibConfigurationService.getPorts()).thenReturn(Optional.of("8080"));

        var mojo = new DockerfileMojo(
            project,
            mock(DockerService.class),
            jibConfigurationService,
            mock(ApplicationConfigurationService.class),
            mock(ExecutorService.class),
            mockSession(project),
            mock(MojoExecution.class)
        );
        mojo.micronautRuntime = "netty";

        var dockerfile = Files.writeString(tempDir.resolve("Dockerfile"), String.join(System.lineSeparator(),
            "ARG BASE_IMAGE_TAG=latest",
            "ARG PORTS_FILE=/tmp/ports",
            "ARG CLASS_NAME_SUFFIX=Application",
            "ARG BASE_IMAGE",
            "FROM ${BASE_IMAGE}"
        ));

        invokeProcessDockerfile(mojo, dockerfile);

        assertEquals(
            java.util.List.of(
                "ARG BASE_IMAGE_TAG=latest",
                "ARG PORTS_FILE=/tmp/ports",
                "ARG CLASS_NAME_SUFFIX=Application",
                "FROM ghcr.io/example/builder:1.0"
            ),
            Files.readAllLines(dockerfile)
        );
    }

    private static MavenProject mockProject(Path tempDir) {
        var project = mock(MavenProject.class);
        var build = mock(Build.class);
        var properties = new Properties();
        tempDir.resolve("target").toFile().mkdirs();
        when(project.getBuild()).thenReturn(build);
        when(project.getProperties()).thenReturn(properties);
        when(project.getArtifacts()).thenReturn(Set.<Artifact>of());
        when(build.getDirectory()).thenReturn(tempDir.resolve("target").toString());
        return project;
    }

    private static MavenSession mockSession(MavenProject project) {
        var session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        when(session.getSystemProperties()).thenReturn(new Properties());
        when(session.getUserProperties()).thenReturn(new Properties());
        return session;
    }

    private static void invokeProcessDockerfile(DockerfileMojo mojo, Path dockerfile) throws IOException, MojoExecutionException {
        try {
            Method method = DockerfileMojo.class.getDeclaredMethod("processDockerfile", java.io.File.class);
            method.setAccessible(true);
            method.invoke(mojo, dockerfile.toFile());
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof MojoExecutionException mojoExecutionException) {
                throw mojoExecutionException;
            }
            if (cause instanceof IOException ioException) {
                throw ioException;
            }
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw new AssertionError(cause);
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }
}
