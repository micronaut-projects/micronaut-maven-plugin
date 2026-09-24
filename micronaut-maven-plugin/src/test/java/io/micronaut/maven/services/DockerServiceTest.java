package io.micronaut.maven.services;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.async.ResultCallback;
import com.github.dockerjava.api.command.CopyArchiveFromContainerCmd;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.ExecCreateCmd;
import com.github.dockerjava.api.command.ExecCreateCmdResponse;
import com.github.dockerjava.api.command.ExecStartCmd;
import com.github.dockerjava.api.command.InfoCmd;
import com.github.dockerjava.api.command.InspectExecCmd;
import com.github.dockerjava.api.command.InspectExecResponse;
import com.github.dockerjava.api.command.InspectImageCmd;
import com.github.dockerjava.api.command.InspectImageResponse;
import com.github.dockerjava.api.command.KillContainerCmd;
import com.github.dockerjava.api.command.LogContainerCmd;
import com.github.dockerjava.api.command.PingCmd;
import com.github.dockerjava.api.command.RemoveContainerCmd;
import com.github.dockerjava.api.command.RemoveImageCmd;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.command.WaitContainerCmd;
import com.github.dockerjava.api.command.WaitContainerResultCallback;
import com.github.dockerjava.api.exception.ConflictException;
import com.github.dockerjava.api.exception.DockerClientException;
import com.github.dockerjava.api.exception.NotFoundException;
import com.github.dockerjava.api.model.Frame;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.Info;
import com.github.dockerjava.api.model.StreamType;
import io.micronaut.maven.jib.JibConfigurationService;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.stubbing.Answer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DockerServiceTest {

    private final DockerClient client = mock(DockerClient.class);
    private final DockerService dockerService = new DockerService(mock(MavenProject.class), mock(JibConfigurationService.class), client);
    private final PingCmd ping = mock(PingCmd.class);

    @BeforeEach
    void theDaemonIsRunning() {
        when(client.pingCmd()).thenReturn(ping);
    }

    @Test
    void createsAContainerWithTheNetworkTheEnvironmentAndTheBinds() {
        var hostConfig = new HostConfig();
        CreateContainerCmd create = stubCreate(hostConfig);

        assertEquals("container", dockerService.createContainer("image", "network", true, Map.of("NAME", "value"), "/host:/container"));

        assertTrue(hostConfig.getPrivileged());
        assertEquals("network", hostConfig.getNetworkMode());
        assertEquals("/container", hostConfig.getBinds()[0].getVolume().getPath());
        verify(create).withEnv(List.of("NAME=value"));
    }

    @Test
    void createsAnUnprivilegedContainerWithoutNetworkNorEnvironment() {
        var hostConfig = new HostConfig();
        CreateContainerCmd create = stubCreate(hostConfig);

        assertEquals("container", dockerService.createContainer("image", null, false, Map.of()));

        assertNull(hostConfig.getPrivileged());
        assertNull(hostConfig.getNetworkMode());
        verify(create, never()).withEnv(anyList());
    }

    @Test
    void failsToCreateAContainerWithoutHostConfig() {
        stubCreate(null);

        assertThrows(DockerClientException.class, () -> dockerService.createContainer("image", null, false, Map.of()));
    }

    @Test
    void runsAPrivilegedImageAndWaitsForIt() throws IOException {
        var hostConfig = new HostConfig();
        stubCreate(hostConfig);
        StartContainerCmd start = stubStart();
        stubExit(0);

        dockerService.runPrivilegedImageAndWait("image", 10, "checkpoint", "/host:/container");

        assertTrue(hostConfig.getPrivileged());
        assertEquals("checkpoint", hostConfig.getNetworkMode());
        verify(start).exec();
    }

    @Test
    void logsTheOutputAndFailsWhenAContainerExitsWithAnError() {
        stubStart();
        stubExit(1);
        LogContainerCmd logs = stubLogs("failure\n");

        var e = assertThrows(IOException.class, () -> dockerService.startAndWait("container", "image", 10));

        assertEquals("Image image exited with code 1", e.getMessage());
        verify(logs).exec(any());
    }

    @Test
    void logsTheOutputAndFailsWhenAContainerDoesNotExitInTime() {
        stubStart();
        WaitContainerResultCallback result = stubWait();
        when(result.awaitStatusCode(anyLong(), any())).thenThrow(new DockerClientException("timeout"));
        LogContainerCmd logs = stubLogs("still running\n");

        var e = assertThrows(IOException.class, () -> dockerService.startAndWait("container", "image", 10));

        assertEquals("Container container did not exit within 10 seconds", e.getMessage());
        verify(logs).exec(any());
    }

    @Test
    void returnsTheExitCodeOfAContainer() throws IOException {
        WaitContainerResultCallback result = stubWait();
        when(result.awaitStatusCode(10, TimeUnit.SECONDS)).thenReturn(143);

        assertEquals(143, dockerService.awaitExit("container", 10));
    }

    @Test
    void signalsAContainer() {
        KillContainerCmd kill = mock(KillContainerCmd.class, RETURNS_SELF);
        when(client.killContainerCmd("container")).thenReturn(kill);

        dockerService.signalContainer("container", "SIGTERM");

        verify(kill).withSignal("SIGTERM");
        verify(kill).exec();
    }

    @Test
    void ignoresSignalsToAContainerThatIsNotRunning() {
        KillContainerCmd kill = mock(KillContainerCmd.class, RETURNS_SELF);
        when(client.killContainerCmd("container")).thenReturn(kill);
        when(kill.exec()).thenThrow(new ConflictException("not running"));

        assertDoesNotThrow(() -> dockerService.signalContainer("container", "SIGTERM"));
    }

    @Test
    void runsACommandInAContainerAndSplitsItsOutputIntoLines() throws IOException {
        ExecCreateCmd create = stubExecCreate();
        stubExecStart(true, "first li", "ne\r\nsecond\nthird");
        stubExecExitCode(3L);
        var lines = new ArrayList<String>();

        assertEquals(3, dockerService.execInContainer("container", 10, lines::add, "bash", "-c", "true"));

        assertEquals(List.of("first line", "second", "third"), lines);
        verify(create).withCmd("bash", "-c", "true");
    }

    @Test
    void returnsMinusOneWhenACommandHasNoExitCode() throws IOException {
        stubExecCreate();
        stubExecStart(true);
        stubExecExitCode(null);

        assertEquals(-1, dockerService.execInContainer("container", 10, line -> { }, "true"));
    }

    @Test
    void failsWhenACommandDoesNotFinishInTime() {
        stubExecCreate();
        stubExecStart(false, "partial");
        var lines = new ArrayList<String>();

        var e = assertThrows(IOException.class, () -> dockerService.execInContainer("container", 0, lines::add, "bash"));

        assertEquals("Command bash did not finish within 0 seconds", e.getMessage());
        assertEquals(List.of("partial"), lines);
    }

    @Test
    void runsAnImageWithAnEntrypointAndCapturesItsOutput() throws IOException {
        CreateContainerCmd create = stubCreate(new HostConfig());
        stubStart();
        stubExit(0);
        stubLogs("openjdk version \"25\"\n", "done");
        RemoveContainerCmd remove = stubRemoveContainer();

        var output = dockerService.runAndCaptureOutput("image", 10, List.of("java", "-version"));

        assertEquals(new DockerService.ContainerOutput(0, "openjdk version \"25\"\ndone"), output);
        verify(create).withEntrypoint(List.of("java", "-version"));
        verify(remove).exec();
    }

    @Test
    void copiesAFileOutOfAContainer(@TempDir Path tempDir) throws IOException {
        stubCopy(tar("app.aot", "cache"));
        Path target = tempDir.resolve("out/app.aot");

        dockerService.copyFileFromContainer("container", "/tmp/app.aot", target);

        assertEquals("cache", Files.readString(target));
    }

    @Test
    void failsToCopyADirectoryOutOfAContainer(@TempDir Path tempDir) throws IOException {
        stubCopy(tar("tmp/", null));

        var e = assertThrows(IOException.class, () -> dockerService.copyFileFromContainer("container", "/tmp", tempDir.resolve("tmp")));

        assertEquals("/tmp is not a file in container container", e.getMessage());
    }

    @Test
    void failsToCopyFromAnEmptyArchive(@TempDir Path tempDir) throws IOException {
        var empty = new ByteArrayOutputStream();
        new TarArchiveOutputStream(empty).close();
        stubCopy(empty.toByteArray());

        var e = assertThrows(IOException.class, () -> dockerService.copyFileFromContainer("container", "/tmp/app.aot", tempDir.resolve("app.aot")));

        assertEquals("/tmp/app.aot is not a file in container container", e.getMessage());
    }

    @Test
    void failsToCopyAMissingFileOutOfAContainer(@TempDir Path tempDir) {
        CopyArchiveFromContainerCmd copy = mock(CopyArchiveFromContainerCmd.class, RETURNS_SELF);
        when(client.copyArchiveFromContainerCmd("container", "/tmp/app.aot")).thenReturn(copy);
        when(copy.exec()).thenThrow(new NotFoundException("missing"));

        var e = assertThrows(IOException.class, () -> dockerService.copyFileFromContainer("container", "/tmp/app.aot", tempDir.resolve("app.aot")));

        assertEquals("/tmp/app.aot does not exist in container container", e.getMessage());
    }

    @Test
    void removesAContainerEvenIfItIsAlreadyGone() {
        RemoveContainerCmd remove = stubRemoveContainer();
        when(remove.exec()).thenThrow(new NotFoundException("gone"));

        assertDoesNotThrow(() -> dockerService.removeContainer("container"));
        verify(remove).withForce(true);
        verify(remove).withRemoveVolumes(true);
    }

    @Test
    void removesAnImage() {
        RemoveImageCmd remove = mock(RemoveImageCmd.class, RETURNS_SELF);
        when(client.removeImageCmd("image")).thenReturn(remove);

        dockerService.removeImage("image");

        verify(remove).withForce(true);
        verify(remove).exec();
    }

    @Test
    void removesAnImageEvenIfItIsAlreadyGone() {
        RemoveImageCmd remove = mock(RemoveImageCmd.class, RETURNS_SELF);
        when(client.removeImageCmd("image")).thenReturn(remove);
        when(remove.exec()).thenThrow(new NotFoundException("gone"));

        assertDoesNotThrow(() -> dockerService.removeImage("image"));
    }

    @Test
    void inspectsAnImage() {
        InspectImageCmd inspect = mock(InspectImageCmd.class, RETURNS_SELF);
        when(client.inspectImageCmd("image")).thenReturn(inspect);
        var image = new InspectImageResponse();
        when(inspect.exec()).thenReturn(image);

        assertSame(image, dockerService.inspectImage("image"));
    }

    @Test
    void returnsThePlatformOfTheDaemonWithGoArchitectureNames() {
        InfoCmd infoCmd = mock(InfoCmd.class);
        when(client.infoCmd()).thenReturn(infoCmd);
        Info info = mock(Info.class);
        when(info.getOsType()).thenReturn("linux");
        when(info.getArchitecture()).thenReturn("x86_64");
        when(infoCmd.exec()).thenReturn(info);

        assertEquals("linux/amd64", dockerService.getDaemonPlatform());
    }

    @Test
    void failsToReturnThePlatformWithoutADaemon() {
        when(ping.exec()).thenThrow(new RuntimeException("connection refused"));

        var e = assertThrows(IllegalStateException.class, dockerService::getDaemonPlatform);

        assertTrue(e.getMessage().startsWith("Cannot connect to the Docker daemon"), e.getMessage());
    }

    @ParameterizedTest
    @CsvSource({"x86_64,amd64", "amd64,amd64", "aarch64,arm64", "arm64,arm64", "s390x,s390x"})
    void usesGoArchitectureNames(String architecture, String expected) {
        assertEquals(expected, DockerService.goArchitecture(architecture));
    }

    private CreateContainerCmd stubCreate(HostConfig hostConfig) {
        CreateContainerCmd create = mock(CreateContainerCmd.class, RETURNS_SELF);
        when(client.createContainerCmd("image")).thenReturn(create);
        when(create.getHostConfig()).thenReturn(hostConfig);
        var response = new CreateContainerResponse();
        response.setId("container");
        when(create.exec()).thenReturn(response);
        return create;
    }

    private StartContainerCmd stubStart() {
        StartContainerCmd start = mock(StartContainerCmd.class, RETURNS_SELF);
        when(client.startContainerCmd("container")).thenReturn(start);
        return start;
    }

    private WaitContainerResultCallback stubWait() {
        WaitContainerCmd wait = mock(WaitContainerCmd.class, RETURNS_SELF);
        when(client.waitContainerCmd("container")).thenReturn(wait);
        WaitContainerResultCallback result = mock(WaitContainerResultCallback.class);
        when(wait.start()).thenReturn(result);
        return result;
    }

    private void stubExit(int exitCode) {
        WaitContainerResultCallback result = stubWait();
        when(result.awaitStatusCode(anyLong(), any())).thenReturn(exitCode);
    }

    private LogContainerCmd stubLogs(String... payloads) {
        LogContainerCmd logs = mock(LogContainerCmd.class, RETURNS_SELF);
        when(client.logContainerCmd("container")).thenReturn(logs);
        when(logs.exec(any())).thenAnswer(frames(true, payloads));
        return logs;
    }

    private ExecCreateCmd stubExecCreate() {
        ExecCreateCmd create = mock(ExecCreateCmd.class, RETURNS_SELF);
        when(client.execCreateCmd("container")).thenReturn(create);
        ExecCreateCmdResponse response = mock(ExecCreateCmdResponse.class);
        when(response.getId()).thenReturn("exec");
        when(create.exec()).thenReturn(response);
        return create;
    }

    private void stubExecStart(boolean complete, String... payloads) {
        ExecStartCmd start = mock(ExecStartCmd.class, RETURNS_SELF);
        when(client.execStartCmd("exec")).thenReturn(start);
        when(start.exec(any())).thenAnswer(frames(complete, payloads));
    }

    private void stubExecExitCode(Long exitCode) {
        InspectExecCmd inspect = mock(InspectExecCmd.class, RETURNS_SELF);
        when(client.inspectExecCmd("exec")).thenReturn(inspect);
        InspectExecResponse response = mock(InspectExecResponse.class);
        when(response.getExitCodeLong()).thenReturn(exitCode);
        when(inspect.exec()).thenReturn(response);
    }

    private RemoveContainerCmd stubRemoveContainer() {
        RemoveContainerCmd remove = mock(RemoveContainerCmd.class, RETURNS_SELF);
        when(client.removeContainerCmd("container")).thenReturn(remove);
        return remove;
    }

    private void stubCopy(byte[] archive) {
        CopyArchiveFromContainerCmd copy = mock(CopyArchiveFromContainerCmd.class, RETURNS_SELF);
        when(client.copyArchiveFromContainerCmd(eq("container"), any())).thenReturn(copy);
        when(copy.exec()).thenReturn(new ByteArrayInputStream(archive));
    }

    /**
     * Sends the payloads to the callback of an asynchronous command, as standard output frames.
     */
    private static Answer<Object> frames(boolean complete, String... payloads) {
        return invocation -> {
            ResultCallback<Frame> callback = invocation.getArgument(0);
            for (String payload : payloads) {
                callback.onNext(new Frame(StreamType.STDOUT, payload.getBytes(StandardCharsets.UTF_8)));
            }
            if (complete) {
                callback.onComplete();
            }
            return callback;
        };
    }

    private static byte[] tar(String name, String content) throws IOException {
        var bytes = new ByteArrayOutputStream();
        try (var tar = new TarArchiveOutputStream(bytes)) {
            var entry = new TarArchiveEntry(name);
            byte[] data = content == null ? new byte[0] : content.getBytes(StandardCharsets.UTF_8);
            entry.setSize(data.length);
            tar.putArchiveEntry(entry);
            tar.write(data);
            tar.closeArchiveEntry();
        }
        return bytes.toByteArray();
    }
}
