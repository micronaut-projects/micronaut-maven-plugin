package io.micronaut.maven;

import com.github.dockerjava.api.command.BuildImageCmd;
import io.micronaut.maven.jib.JibConfigurationService;
import io.micronaut.maven.services.ApplicationConfigurationService;
import io.micronaut.maven.services.DockerService;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.filtering.MavenFilteringException;
import org.apache.maven.shared.filtering.MavenReaderFilter;
import org.apache.maven.shared.filtering.MavenReaderFilterRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junitpioneer.jupiter.RestoreSystemProperties;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RestoreSystemProperties
class DockerCracMojoTest {

    @Test
    void resolvesPinnedCracJdkForJava25OnAmd64(@TempDir Path tempDir) throws MojoExecutionException {
        var fixtures = new Fixtures(tempDir);

        assertEquals(
            "https://cdn.azul.com/zulu/bin/zulu25.32.23-ca-crac-jdk25.0.2-linux_x64.tar.gz",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadUrl", DockerCracMojo.X86_64_ARCH)
        );
        assertEquals(
            "2ebb784450f158e628f5717034aac3126591a6ef03498290297f2f46d8f886b1",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadSha256", DockerCracMojo.X86_64_ARCH)
        );
    }

    @Test
    void resolvesPinnedCracJdkFromReleaseVersion(@TempDir Path tempDir) throws MojoExecutionException {
        var fixtures = new Fixtures(tempDir);
        setField(fixtures.mojo, "cracJavaVersion", "21.0.10");

        assertEquals(
            "https://cdn.azul.com/zulu/bin/zulu21.48.17-ca-crac-jdk21.0.10-linux_aarch64.tar.gz",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadUrl", DockerCracMojo.ARM_ARCH)
        );
        assertEquals(
            "17c88f5112d9782255826fb860217a7d56686451e1629a98695ffd63718d3366",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadSha256", DockerCracMojo.ARM_ARCH)
        );
    }

    @Test
    void usesExplicitCracJdkOverride(@TempDir Path tempDir) throws MojoExecutionException {
        var fixtures = new Fixtures(tempDir);
        setField(fixtures.mojo, "cracJdkDownloadUrl", "https://example.com/custom-crac.tar.gz");
        setField(fixtures.mojo, "cracJdkDownloadSha256", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");

        assertEquals(
            "https://example.com/custom-crac.tar.gz",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadUrl", DockerCracMojo.X86_64_ARCH)
        );
        assertEquals(
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            invokeStringMethod(fixtures.mojo, "cracJdkDownloadSha256", DockerCracMojo.X86_64_ARCH)
        );
    }

    @Test
    void rejectsPartialCracJdkOverride(@TempDir Path tempDir) {
        var fixtures = new Fixtures(tempDir);
        setField(fixtures.mojo, "cracJdkDownloadUrl", "https://example.com/custom-crac.tar.gz");

        var exception = assertThrows(
            MojoExecutionException.class,
            () -> invokeStringMethod(fixtures.mojo, "cracJdkDownloadUrl", DockerCracMojo.X86_64_ARCH)
        );

        assertEquals(
            DockerCracMojo.CRAC_JDK_DOWNLOAD_URL_PROPERTY + " and " + DockerCracMojo.CRAC_JDK_DOWNLOAD_SHA256_PROPERTY
                + " must be configured together",
            exception.getMessage()
        );
    }

    @Test
    void buildCheckpointDockerfilePassesPinnedBuildArgs(@TempDir Path tempDir) throws Exception {
        var fixtures = new Fixtures(tempDir);

        invokeBuildCheckpointDockerfile(fixtures.mojo);

        verify(fixtures.buildImageCmd).withBuildArg(
            "CRAC_JDK_URL",
            "https://cdn.azul.com/zulu/bin/zulu25.32.23-ca-crac-jdk25.0.2-linux_x64.tar.gz"
        );
        verify(fixtures.buildImageCmd).withBuildArg(
            "CRAC_JDK_SHA256",
            "2ebb784450f158e628f5717034aac3126591a6ef03498290297f2f46d8f886b1"
        );
    }

    @Test
    void buildCheckpointDockerfileUsesAarch64PinnedArtifactsForArm64Hosts(@TempDir Path tempDir) throws Exception {
        var fixtures = new Fixtures(tempDir);
        System.setProperty("os.arch", "arm64");
        setField(fixtures.mojo, "cracArchitecture", null);

        invokeBuildCheckpointDockerfile(fixtures.mojo);

        verify(fixtures.buildImageCmd).withBuildArg(
            "CRAC_JDK_URL",
            "https://cdn.azul.com/zulu/bin/zulu25.32.23-ca-crac-jdk25.0.2-linux_aarch64.tar.gz"
        );
        verify(fixtures.buildImageCmd).withBuildArg(
            "CRAC_JDK_SHA256",
            "83f4621c04cf8f8d2ce8ce82e7505b85897d9e5b4cadb5472a1c679bc27a41bd"
        );
    }

    @Test
    void rejectsCracJdkOverrideWithEmbeddedCredentials(@TempDir Path tempDir) {
        var fixtures = new Fixtures(tempDir);
        setField(fixtures.mojo, "cracJdkDownloadUrl", "https://user:secret@example.com/custom-crac.tar.gz");
        setField(fixtures.mojo, "cracJdkDownloadSha256", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");

        var exception = assertThrows(
            MojoExecutionException.class,
            () -> invokeStringMethod(fixtures.mojo, "cracJdkDownloadUrl", DockerCracMojo.X86_64_ARCH)
        );

        assertEquals("crac.jdk.download.url must not include embedded credentials", exception.getMessage());
    }

    private static void invokeBuildCheckpointDockerfile(DockerCracMojo mojo) throws Exception {
        Method buildCheckpointDockerfile = DockerCracMojo.class.getDeclaredMethod("buildCheckpointDockerfile");
        buildCheckpointDockerfile.setAccessible(true);
        try {
            buildCheckpointDockerfile.invoke(mojo);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof Exception exception) {
                throw exception;
            }
            throw e;
        }
    }

    private static String invokeStringMethod(DockerCracMojo mojo, String methodName, String argument) throws MojoExecutionException {
        try {
            Method method = DockerCracMojo.class.getDeclaredMethod(methodName, String.class);
            method.setAccessible(true);
            return (String) method.invoke(mojo, argument);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof MojoExecutionException exception) {
                throw exception;
            }
            throw new RuntimeException(cause);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private static void setField(Object target, String name, Object value) {
        try {
            Field field = target.getClass().getDeclaredField(name);
            field.setAccessible(true);
            field.set(target, value);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private static final class Fixtures {
        private final BuildImageCmd buildImageCmd;
        private final DockerCracMojo mojo;

        private Fixtures(Path tempDir) {
            MavenProject project = mock(MavenProject.class);
            MavenSession session = mock(MavenSession.class);
            MojoExecution execution = mock(MojoExecution.class);
            JibConfigurationService jibConfigurationService = mock(JibConfigurationService.class);
            ApplicationConfigurationService applicationConfigurationService = mock(ApplicationConfigurationService.class);
            DockerService dockerService = mock(DockerService.class);
            MavenReaderFilter mavenReaderFilter = mock(MavenReaderFilter.class);
            buildImageCmd = mock(BuildImageCmd.class, RETURNS_SELF);

            Build build = new Build();
            build.setDirectory(tempDir.resolve("target").toString());

            when(session.getCurrentProject()).thenReturn(project);
            when(session.getUserProperties()).thenReturn(new Properties());
            when(session.getSystemProperties()).thenReturn(new Properties());
            when(project.getProperties()).thenReturn(new Properties());
            when(project.getBuild()).thenReturn(build);
            when(project.getBasedir()).thenReturn(tempDir.toFile());
            when(project.getArtifactId()).thenReturn("demo");
            when(jibConfigurationService.getFromImage()).thenReturn(Optional.empty());
            when(dockerService.buildImageCmd()).thenReturn(buildImageCmd);
            when(dockerService.buildImage(buildImageCmd)).thenReturn("image-id");
            try {
                when(mavenReaderFilter.filter(any(MavenReaderFilterRequest.class))).thenAnswer(
                    invocation -> invocation.getArgument(0, MavenReaderFilterRequest.class).getFrom()
                );
            } catch (MavenFilteringException e) {
                throw new RuntimeException(e);
            }

            try {
                Path dockerfile = tempDir.resolve("DockerfileCracCheckpoint");
                Files.writeString(dockerfile, "FROM ${BASE_IMAGE}\n");
                when(dockerService.loadDockerfileAsResource(DockerfileMojo.DOCKERFILE_CRAC_CHECKPOINT)).thenReturn(dockerfile.toFile());
                for (String scriptName : new String[] {
                    DockerCracMojo.CHECKPOINT_SCRIPT_NAME,
                    DockerCracMojo.WARMUP_SCRIPT_NAME,
                    DockerCracMojo.RUN_SCRIPT_NAME
                }) {
                    Path script = tempDir.resolve(scriptName);
                    Files.writeString(script, "#!/bin/sh\n");
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            mojo = new DockerCracMojo(
                project,
                jibConfigurationService,
                applicationConfigurationService,
                dockerService,
                mavenReaderFilter,
                session,
                execution
            );
            mojo.mainClass = "example.Application";
            setField(mojo, "cracJavaVersion", "25");
            setField(mojo, "cracOs", DockerCracMojo.DEFAULT_CRAC_OS);
            setField(mojo, "cracArchitecture", DockerCracMojo.X86_64_ARCH);
            setField(mojo, "readinessCommand", DockerCracMojo.DEFAULT_READINESS_COMMAND);
        }
    }
}
