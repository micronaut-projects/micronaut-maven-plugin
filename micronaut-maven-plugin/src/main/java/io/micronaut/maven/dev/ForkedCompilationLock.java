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
package io.micronaut.maven.dev;

import io.micronaut.maven.core.MojoUtils;
import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.ExecutionEvent;
import org.apache.maven.execution.ExecutionListener;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Path;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Holds the {@link OutputLock output lock} across the lifecycle {@code mn:dev} and {@code mn:test} fork before they
 * launch, {@code process-classes} and {@code process-test-classes}: its compilations write the class outputs that a
 * running {@code mn:test} or {@code mn:dev} of the same project compiles into and reads. A goal cannot take a lock
 * around the lifecycle Maven forks before running it, so this participant, active when the plugin is declared with
 * {@code extensions} set to true, as the Micronaut parent POM declares it, wraps the build's execution listener
 * and takes the lock when such a fork starts, releasing it when the fork ends.
 *
 * @author graemerocher
 * @since 5.1.0
 */
public class ForkedCompilationLock extends AbstractMavenLifecycleParticipant {

    private static final Logger LOG = LoggerFactory.getLogger(ForkedCompilationLock.class);
    private static final String DEV_GOAL = "dev";
    private static final String TEST_GOAL = "test";

    @Override
    public void afterProjectsRead(MavenSession session) {
        if (session.getGoals().stream().anyMatch(ForkedCompilationLock::isDevelopmentGoal)) {
            ExecutionListener listener = session.getRequest().getExecutionListener();
            if (!(listener instanceof Listener)) {
                session.getRequest().setExecutionListener(new Listener(listener));
            }
        }
    }

    /**
     * Whether a goal of the command line may be {@code mn:dev} or {@code mn:test}: a plugin goal, not the {@code test}
     * phase, named {@code dev} or {@code test}. The fork itself is checked to be of this plugin.
     *
     * @param goal the goal
     * @return whether it may be
     */
    static boolean isDevelopmentGoal(String goal) {
        int colon = goal.lastIndexOf(':');
        if (colon < 0) {
            return false;
        }
        String name = goal.substring(colon + 1);
        return DEV_GOAL.equals(name) || TEST_GOAL.equals(name);
    }

    /**
     * Whether an execution is {@code mn:dev} or {@code mn:test}, whose forked lifecycle compiles.
     *
     * @param execution the execution
     * @return whether it is
     */
    static boolean isDevelopmentExecution(MojoExecution execution) {
        return execution != null
            && MojoUtils.THIS_PLUGIN.equals(execution.getGroupId() + ":" + execution.getArtifactId())
            && (DEV_GOAL.equals(execution.getGoal()) || TEST_GOAL.equals(execution.getGoal()));
    }

    /**
     * The lock files of a fork: those of each project of the build that declares the plugin, among which the goal
     * selects the application, with the tests' output for {@code mn:test}.
     *
     * @param session the session
     * @param execution the execution whose lifecycle forks
     * @return the lock files
     */
    static Set<Path> lockFiles(MavenSession session, MojoExecution execution) {
        boolean tests = TEST_GOAL.equals(execution.getGoal());
        Set<Path> files = new LinkedHashSet<>();
        List<MavenProject> projects = session.getProjects();
        for (MavenProject project : projects) {
            if (MojoUtils.hasMicronautMavenPlugin(project)) {
                files.addAll(OutputLock.files(project, tests));
            }
        }
        return files;
    }

    /**
     * The build's execution listener, which it delegates to, holding the lock while a development goal's fork runs.
     */
    static final class Listener implements ExecutionListener {

        private final ExecutionListener delegate;
        private final Map<MojoExecution, OutputLock.Hold> holds = new IdentityHashMap<>();

        Listener(ExecutionListener delegate) {
            this.delegate = delegate;
        }

        @Override
        public void forkStarted(ExecutionEvent event) {
            MojoExecution execution = event.getMojoExecution();
            if (isDevelopmentExecution(execution)) {
                Set<Path> files = lockFiles(event.getSession(), execution);
                if (!files.isEmpty()) {
                    OutputLock.Hold hold = OutputLock.lock(files, LOG::info);
                    OutputLock.Hold previous;
                    synchronized (holds) {
                        previous = holds.put(execution, hold);
                    }
                    if (previous != null) {
                        previous.close();
                    }
                }
            }
            if (delegate != null) {
                delegate.forkStarted(event);
            }
        }

        @Override
        public void forkSucceeded(ExecutionEvent event) {
            release(event);
            if (delegate != null) {
                delegate.forkSucceeded(event);
            }
        }

        @Override
        public void forkFailed(ExecutionEvent event) {
            release(event);
            if (delegate != null) {
                delegate.forkFailed(event);
            }
        }

        @Override
        public void mojoStarted(ExecutionEvent event) {
            // the fork is over by the time the goal itself starts
            release(event);
            if (delegate != null) {
                delegate.mojoStarted(event);
            }
        }

        @Override
        public void mojoFailed(ExecutionEvent event) {
            release(event);
            if (delegate != null) {
                delegate.mojoFailed(event);
            }
        }

        @Override
        public void sessionEnded(ExecutionEvent event) {
            List<OutputLock.Hold> remaining;
            synchronized (holds) {
                remaining = List.copyOf(holds.values());
                holds.clear();
            }
            remaining.forEach(OutputLock.Hold::close);
            if (delegate != null) {
                delegate.sessionEnded(event);
            }
        }

        private void release(ExecutionEvent event) {
            MojoExecution execution = event.getMojoExecution();
            if (execution == null) {
                return;
            }
            OutputLock.Hold hold;
            synchronized (holds) {
                hold = holds.remove(execution);
            }
            if (hold != null) {
                hold.close();
            }
        }

        @Override
        public void projectDiscoveryStarted(ExecutionEvent event) {
            if (delegate != null) {
                delegate.projectDiscoveryStarted(event);
            }
        }

        @Override
        public void sessionStarted(ExecutionEvent event) {
            if (delegate != null) {
                delegate.sessionStarted(event);
            }
        }

        @Override
        public void projectSkipped(ExecutionEvent event) {
            if (delegate != null) {
                delegate.projectSkipped(event);
            }
        }

        @Override
        public void projectStarted(ExecutionEvent event) {
            if (delegate != null) {
                delegate.projectStarted(event);
            }
        }

        @Override
        public void projectSucceeded(ExecutionEvent event) {
            if (delegate != null) {
                delegate.projectSucceeded(event);
            }
        }

        @Override
        public void projectFailed(ExecutionEvent event) {
            if (delegate != null) {
                delegate.projectFailed(event);
            }
        }

        @Override
        public void mojoSkipped(ExecutionEvent event) {
            if (delegate != null) {
                delegate.mojoSkipped(event);
            }
        }

        @Override
        public void mojoSucceeded(ExecutionEvent event) {
            if (delegate != null) {
                delegate.mojoSucceeded(event);
            }
        }

        @Override
        public void forkedProjectStarted(ExecutionEvent event) {
            if (delegate != null) {
                delegate.forkedProjectStarted(event);
            }
        }

        @Override
        public void forkedProjectSucceeded(ExecutionEvent event) {
            if (delegate != null) {
                delegate.forkedProjectSucceeded(event);
            }
        }

        @Override
        public void forkedProjectFailed(ExecutionEvent event) {
            if (delegate != null) {
                delegate.forkedProjectFailed(event);
            }
        }
    }
}
