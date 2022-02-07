/*
 * Copyright 2003-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.build.aot;

import io.micronaut.aot.std.sourcegen.AbstractStaticServiceLoaderSourceGenerator;
import io.micronaut.aot.std.sourcegen.KnownMissingTypesSourceGenerator;
import io.micronaut.build.services.CompilerService;
import io.micronaut.build.services.ExecutorService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

import javax.inject.Inject;
import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

@Mojo(name = "aot-analysis", defaultPhase = LifecyclePhase.PACKAGE, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class MicronautAotOptimizerMojo extends AbstractMicronautAotCliMojo {

    @Parameter(property = "project.baseDir", defaultValue = "${project.build.directory}", required = true)
    private File baseDirectory;

    @Parameter(property = "micronaut.aot.config", defaultValue = "aot.properties")
    private File configFile;

    @Inject
    public MicronautAotOptimizerMojo(CompilerService compilerService, ExecutorService executorService) {
        super(compilerService, executorService);
    }

    @Override
    protected List<String> getExtraArgs() throws MojoExecutionException {
        List<String> args = new ArrayList<>();
        args.add("--output");
        File outputDirectory = outputFile("generated");
        args.add(outputDirectory.getAbsolutePath());
        File configFile = writeEffectiveConfigFile();
        args.add("--config");
        args.add(configFile.getAbsolutePath());
        return args;
    }

    private File writeEffectiveConfigFile() throws MojoExecutionException {
        File userProvidedFile = this.configFile == null ? new File(baseDirectory, "aot.properties") : this.configFile;
        Properties props = new Properties();
        if (userProvidedFile.exists()) {
            try (InputStream in = new FileInputStream(userProvidedFile)) {
                props.load(in);
            } catch (IOException e) {
                throw new MojoExecutionException("Unable to parse configuration file", e);
            }
        }
        if (!props.containsKey(KnownMissingTypesSourceGenerator.OPTION.key())) {
            props.put(KnownMissingTypesSourceGenerator.OPTION.key(), String.join(",", Constants.TYPES_TO_CHECK));
        }
        if (!props.containsKey(AbstractStaticServiceLoaderSourceGenerator.SERVICE_TYPES)) {
            props.put(AbstractStaticServiceLoaderSourceGenerator.SERVICE_TYPES, String.join(",", Constants.SERVICE_TYPES));
        }
        File effectiveConfig = outputFile("effective-aot.properties");
        try (OutputStream out = new FileOutputStream(effectiveConfig)) {
            props.store(out, "Effective AOT configuration");
        } catch (IOException e) {
            throw new MojoExecutionException("Unable to parse configuration file", e);
        }
        return effectiveConfig;
    }

}
