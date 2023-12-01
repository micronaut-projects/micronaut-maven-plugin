/*
 * Copyright 2017-2021 original authors
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
package io.micronaut.maven.testresources;

import io.micronaut.maven.JansiLog;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.ContextEnabled;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.plugins.annotations.Parameter;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Base mojo for Micronaut test resources service handling.
 */
public abstract class AbstractTestResourcesMojo extends TestResourcesConfiguration implements Mojo, ContextEnabled {

    private static final String DEFAULT_CLASSPATH_INFERENCE = "true";
    private static final String DEFAULT_CLIENT_TIMEOUT = "60";

    /**
     * Instance logger.
     */
    protected Log log;

    /**
     * Plugin container context.
     */
    protected Map<?, ?> pluginContext;

    @Parameter(defaultValue = "${project.build.directory}", required = true)
    protected File buildDirectory;

    /**
     * Micronaut Test Resources version. Should be defined by the Micronaut BOM, but this parameter can be used to
     * define a different version.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "version", required = true)
    protected String testResourcesVersion;

    /**
     * If set to true, Micronaut will attempt to infer which dependencies should be added to the Test Resources server
     * classpath, based on the project dependencies. In general the result will consist of modules from the
     * test-resources project, but it may consist of additional entries, for example database drivers.
     */
    @Parameter(defaultValue = DEFAULT_CLASSPATH_INFERENCE)
    protected Boolean classpathInference = Boolean.valueOf(DEFAULT_CLASSPATH_INFERENCE);

    /**
     * Additional dependencies to add to the Test Resources server classpath when not using classpath inference, or when
     * the inference doesn't produce the desired result.
     */
    @Parameter
    protected List<Dependency> testResourcesDependencies;

    /**
     * By default, the Test Resources server will be started on a random (available) port, but it can be set a fixed port
     * by using this parameter.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "port")
    protected Integer explicitPort;

    /**
     * Configures the maximum amount of time to wait for the server to start a test resource. Some containeres may take
     * a long amount of time to start with slow internet connections.
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "client-timeout", defaultValue = DEFAULT_CLIENT_TIMEOUT)
    protected Integer clientTimeout = Integer.valueOf(DEFAULT_CLIENT_TIMEOUT);

    /**
     * Configures the duration after which the test resources service will automatically shut down if it doesn't
     * get any request.
     * @since 4.2.0
     */
    @Parameter(property = CONFIG_PROPERTY_PREFIX + "server-idle-timeout-minutes")
    protected Integer serverIdleTimeoutMinutes = Integer.valueOf(DEFAULT_CLIENT_TIMEOUT);

    public static Path serverSettingsDirectoryOf(Path buildDir) {
        return buildDir.resolve("../.micronaut/test-resources");
    }

    @Override
    public void setLog(Log log) {
        this.log = new JansiLog(log);
    }

    @Override
    public Log getLog() {
        if (log == null) {
            log = new SystemStreamLog();
        }
        return log;
    }

    @Override
    public Map<?, ?> getPluginContext() {
        return pluginContext;
    }

    @Override
    public void setPluginContext(Map pluginContext) {
        this.pluginContext = pluginContext;
    }

}
