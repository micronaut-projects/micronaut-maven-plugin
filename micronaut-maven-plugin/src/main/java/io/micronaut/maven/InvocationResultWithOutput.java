/*
 * Copyright 2017-2025 original authors
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

import org.apache.maven.shared.invoker.InvocationOutputHandler;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.utils.cli.CommandLineException;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * An {@link InvocationResult} that includes the output and error. This allows callers to conditionally display the
 * output depending on the exit code.
 *
 * @param result the invocation result.
 * @param outputHandler the output handler.
 */
public record InvocationResultWithOutput(InvocationResult result,
                                         PerGoalOutputHandler outputHandler) implements InvocationResult {

    @Override
    public CommandLineException getExecutionException() {
        return result.getExecutionException();
    }

    @Override
    public int getExitCode() {
        return result.getExitCode();
    }

    /**
     * An {@link InvocationOutputHandler} that can provide the output of each goals separately.
     */
    public static class PerGoalOutputHandler implements InvocationOutputHandler {

        private static final Pattern GOAL_PATTERN = Pattern.compile(
                "---\\s+([^:]+):([^:]+):([^\\s]+)\\s+\\(([^\\)]+)\\)\\s+@\\s+([^\\s]+)\\s+---");

        // Matches the start or content of the trailing summary (BUILD SUCCESS/FAILURE, Total time, Finished at, etc.)
        private static final Pattern BUILD_SUMMARY_PATTERN = Pattern.compile("^BUILD\\s(SUCCESS|FAILURE|ERROR)$");
        private static final Pattern SEPARATOR_PATTERN = Pattern.compile("^-{72}$");

        private final Map<String, List<String>> perGoalOutput = new LinkedHashMap<>();
        private final List<String> fullOutput = new ArrayList<>();

        private String currentGoal = null;
        private boolean skippingSummary = false;
        private boolean separatorFoundBefore = false;
        private final Deque<String> recentLines = new ArrayDeque<>(6); // buffer for dashed lines

        @Override
        public void consumeLine(String line) {
            if (line == null) {
                return;
            }
            // Strip standard Maven log prefixes like [INFO], [WARNING]
            String cleanLine = line.replaceFirst("^\\[\\w+\\]\\s*", "").trim();
            if (cleanLine.isEmpty()) {
                return;
            }
            fullOutput.add(cleanLine);

            // Once we start the build summary section — skip all further lines
            if (skippingSummary) {
                return;
            }

            if (separatorFoundBefore) {
                separatorFoundBefore = false;
                if (BUILD_SUMMARY_PATTERN.matcher(line).matches()) {
                    skippingSummary = true;
                    return;
                }
            } else if (SEPARATOR_PATTERN.matcher(cleanLine).matches()) {
                separatorFoundBefore = true;
                return;
            }

            Matcher goalMatcher = GOAL_PATTERN.matcher(line);
            if (goalMatcher.find()) {
                currentGoal = goalMatcher.group(1) + ":" + goalMatcher.group(3); // e.g., compiler:compile
                perGoalOutput.computeIfAbsent(currentGoal, k -> new ArrayList<>()).add(cleanLine);
            } else if (currentGoal != null) {
                perGoalOutput.get(currentGoal).add(cleanLine);
            }

        }

        /**
         * @param pluginGoalKey plugin/goal formatted as pluginId:goal, e.g.: compiler:compile
         * @return the output of the given plugin goal key, with log level prefixes and build summary stripped out.
         */
        public List<String> getOutput(String pluginGoalKey) {
            return perGoalOutput.getOrDefault(pluginGoalKey, Collections.emptyList());
        }

        /**
         * @return the complete output, without log prefixes or trailing summary lines.
         */
        public List<String> getOutput() {
            return Collections.unmodifiableList(fullOutput);
        }

    }
}
