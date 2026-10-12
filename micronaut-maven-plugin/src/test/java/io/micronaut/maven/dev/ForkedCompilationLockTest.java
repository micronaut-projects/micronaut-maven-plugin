package io.micronaut.maven.dev;

import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.ExecutionEvent;
import org.apache.maven.execution.ExecutionListener;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ForkedCompilationLockTest {

    @TempDir
    Path directory;

    @Test
    void theGoalsOfDevelopmentAndTestModeAreRecognised() {
        assertTrue(ForkedCompilationLock.isDevelopmentGoal("mn:dev"));
        assertTrue(ForkedCompilationLock.isDevelopmentGoal("mn:test"));
        assertTrue(ForkedCompilationLock.isDevelopmentGoal("io.micronaut.maven:micronaut-maven-plugin:5.1.0:test"));
        assertTrue(ForkedCompilationLock.isDevelopmentGoal("mn:dev@local"));
        assertTrue(ForkedCompilationLock.isDevelopmentGoal("io.micronaut.maven:micronaut-maven-plugin:test@continuous"));
        assertFalse(ForkedCompilationLock.isDevelopmentGoal("mn:run@local"));
        // the phase, not the goal
        assertFalse(ForkedCompilationLock.isDevelopmentGoal("test"));
        assertFalse(ForkedCompilationLock.isDevelopmentGoal("compile"));
        assertFalse(ForkedCompilationLock.isDevelopmentGoal("mn:run"));
    }

    @Test
    void theListenerIsWrappedOnlyForADevelopmentGoal() {
        ExecutionListener original = mock(ExecutionListener.class);
        MavenSession build = session(List.of("clean", "test"), original, List.of());
        new ForkedCompilationLock().afterProjectsRead(build);
        assertSame(original, build.getRequest().getExecutionListener());

        MavenSession dev = session(List.of("mn:dev"), original, List.of());
        new ForkedCompilationLock().afterProjectsRead(dev);
        assertInstanceOf(ForkedCompilationLock.Listener.class, dev.getRequest().getExecutionListener());
        // once
        ExecutionListener wrapped = dev.getRequest().getExecutionListener();
        new ForkedCompilationLock().afterProjectsRead(dev);
        assertSame(wrapped, dev.getRequest().getExecutionListener());
    }

    @Test
    void theForkOfTheTestGoalHoldsTheLocksOfTheApplicationsProjectUntilItEnds() throws Exception {
        MavenProject app = project("app", true);
        MavenProject library = project("library", false);
        ExecutionListener original = mock(ExecutionListener.class);
        MavenSession session = session(List.of("mn:test"), original, List.of(library, app));
        MojoExecution execution = execution("io.micronaut.maven", "micronaut-maven-plugin", "test");

        assertEquals(Set.of(directory.resolve("app/target/classes.micronaut-dev.lock"), directory.resolve("app/target/test-classes.micronaut-dev.lock")),
            ForkedCompilationLock.lockFiles(session, execution));

        ForkedCompilationLock.Listener listener = new ForkedCompilationLock.Listener(original);
        ExecutionEvent event = event(session, execution);
        listener.forkStarted(event);
        verify(original).forkStarted(event);

        Path lockFile = directory.resolve("app/target/classes.micronaut-dev.lock");
        CountDownLatch locked = new CountDownLatch(1);
        Thread launcher = new Thread(() -> {
            try (OutputLock.Hold hold = OutputLock.lock(List.of(lockFile), message -> { })) {
                locked.countDown();
            }
        });
        launcher.start();
        assertFalse(locked.await(200, TimeUnit.MILLISECONDS));

        listener.forkSucceeded(event);
        verify(original).forkSucceeded(event);
        assertTrue(locked.await(5, TimeUnit.SECONDS));
        launcher.join(5000);
    }

    @Test
    void anotherPluginsForkLocksNothing() throws Exception {
        MavenProject app = project("app", true);
        MavenSession session = session(List.of("mn:dev"), null, List.of(app));
        MojoExecution execution = execution("org.apache.maven.plugins", "maven-javadoc-plugin", "test");
        ForkedCompilationLock.Listener listener = new ForkedCompilationLock.Listener(null);

        listener.forkStarted(event(session, execution));

        // not held: taken at once
        OutputLock.lock(List.of(directory.resolve("app/target/classes.micronaut-dev.lock")), message -> {
            throw new AssertionError("waited");
        }).close();
        listener.sessionEnded(event(session, execution));
    }

    @Test
    void theEndOfTheSessionReleasesAForkThatNeverEnded() throws Exception {
        MavenProject app = project("app", true);
        MavenSession session = session(List.of("mn:dev"), null, List.of(app));
        MojoExecution execution = execution("io.micronaut.maven", "micronaut-maven-plugin", "dev");
        ForkedCompilationLock.Listener listener = new ForkedCompilationLock.Listener(null);

        listener.forkStarted(event(session, execution));
        listener.sessionEnded(event(session, null));

        OutputLock.lock(List.of(directory.resolve("app/target/classes.micronaut-dev.lock")), message -> {
            throw new AssertionError("waited");
        }).close();
    }

    private MavenProject project(String artifactId, boolean withPlugin) {
        MavenProject project = new MavenProject();
        project.setGroupId("example");
        project.setArtifactId(artifactId);
        project.getBuild().setDirectory(directory.resolve(artifactId + "/target").toString());
        project.getBuild().setOutputDirectory(directory.resolve(artifactId + "/target/classes").toString());
        project.getBuild().setTestOutputDirectory(directory.resolve(artifactId + "/target/test-classes").toString());
        if (withPlugin) {
            Plugin plugin = new Plugin();
            plugin.setGroupId("io.micronaut.maven");
            plugin.setArtifactId("micronaut-maven-plugin");
            project.getBuild().addPlugin(plugin);
        }
        return project;
    }

    private static MavenSession session(List<String> goals, ExecutionListener listener, List<MavenProject> projects) {
        MavenExecutionRequest request = new DefaultMavenExecutionRequest();
        request.setGoals(goals);
        request.setExecutionListener(listener);
        MavenSession session = mock(MavenSession.class);
        when(session.getRequest()).thenReturn(request);
        when(session.getGoals()).thenReturn(goals);
        when(session.getProjects()).thenReturn(projects);
        return session;
    }

    private static MojoExecution execution(String groupId, String artifactId, String goal) {
        PluginDescriptor plugin = new PluginDescriptor();
        plugin.setGroupId(groupId);
        plugin.setArtifactId(artifactId);
        MojoDescriptor descriptor = new MojoDescriptor();
        descriptor.setPluginDescriptor(plugin);
        descriptor.setGoal(goal);
        return new MojoExecution(descriptor);
    }

    private static ExecutionEvent event(MavenSession session, MojoExecution execution) {
        ExecutionEvent event = mock(ExecutionEvent.class);
        when(event.getSession()).thenReturn(session);
        when(event.getMojoExecution()).thenReturn(execution);
        return event;
    }
}
