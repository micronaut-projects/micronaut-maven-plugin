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
package io.micronaut.maven.jsonschema;

import io.micronaut.jsonschema.configuration.validator.DependencyInjectionError;
import io.micronaut.maven.AbstractMicronautMojo;
import io.micronaut.maven.services.CompilerService;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.graph.Dependency;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Base mojo for validating Micronaut configuration using JSON Schema.
 */
abstract class AbstractConfigurationValidationMojo extends AbstractMicronautMojo {

    private static final List<String> DEFAULT_CACHE_IGNORE = List.of(
        "META-INF/*",
        "logback.xml",
        "logback-test.xml"
    );

    protected final MavenProject project;
    protected final CompilerService compilerService;

    @Parameter
    private ConfigurationValidationConfiguration configurationValidation;

    @Inject
    protected AbstractConfigurationValidationMojo(MavenProject project, CompilerService compilerService) {
        this.project = project;
        this.compilerService = compilerService;
    }

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        ConfigurationValidationConfiguration cfg = configurationValidation;
        if (cfg == null) {
            cfg = new ConfigurationValidationConfiguration();
        }

        if (!isEnabled(cfg)) {
            return;
        }
        ConfigurationValidationConfiguration.ValidationSet set = validationSet(cfg);
        if (set != null && Boolean.FALSE.equals(set.getEnabled())) {
            return;
        }

        try {
            doValidate(cfg, set);
        } catch (MojoFailureException | MojoExecutionException e) {
            throw e;
        } catch (Exception e) {
            throw new MojoExecutionException("Configuration validation failed", e);
        }
    }

    protected abstract String scenarioName();

    protected abstract ConfigurationValidationConfiguration.ValidationSet validationSet(ConfigurationValidationConfiguration cfg);

    protected abstract List<String> defaultEnvironments();

    protected abstract List<String> defaultClasspath(MavenProject project);

    protected abstract String[] dependencyScopes();

    /**
     * Default resource directories for origin rewriting.
     *
     * @return Resource directories (typically relative to the project base directory)
     */
    protected abstract List<Path> defaultResourceDirectories();

    private void doValidate(ConfigurationValidationConfiguration cfg, ConfigurationValidationConfiguration.ValidationSet set)
        throws MojoExecutionException, MojoFailureException, IOException {

        List<String> environments = computeEnvironments(set, defaultEnvironments());
        List<String> suppressions = cfg.getSuppressions() == null ? List.of() : List.copyOf(cfg.getSuppressions());
        List<String> suppressInjectErrors = cfg.getSuppressInjectErrors() == null
            ? List.of()
            : List.copyOf(cfg.getSuppressInjectErrors());
        boolean failOnNotPresent = cfg.getFailOnNotPresent() == null || cfg.getFailOnNotPresent();
        boolean deduceEnvironments = cfg.getDeduceEnvironments() != null && cfg.getDeduceEnvironments();
        boolean validateDependencyInjection = cfg.getValidateDependencyInjection() != null && cfg.getValidateDependencyInjection();
        ConfigurationValidationFormat format = parseFormat(cfg.getFormat());

        Path outputDir = determineOutputDir(cfg, set);
        Path cacheFile = outputDir.resolve(".cache.properties");

        boolean cacheEnabled = cfg.getCacheEnabled() == null || cfg.getCacheEnabled();

        List<String> classpathElements = computeClasspathElements(set);
        String classpath = String.join(File.pathSeparator, classpathElements);
        String classpathFingerprint = cacheEnabled && validateDependencyInjection
            ? ConfigurationValidationCache.fingerprintClasspath(classpathElements)
            : "classpath-fingerprint-disabled";

        String inputsFingerprint = String.join("|",
            scenarioName(),
            environments.toString(),
            suppressions.toString(),
            suppressInjectErrors.toString(),
            Boolean.toString(failOnNotPresent),
            Boolean.toString(deduceEnvironments),
            Boolean.toString(validateDependencyInjection),
            format.name(),
            classpath,
            classpathFingerprint
        );
        List<String> cacheIgnore = cfg.getCacheIgnore() == null ? DEFAULT_CACHE_IGNORE : cfg.getCacheIgnore();

        List<Path> cacheResourceDirs = computeCacheResourceDirectories(set);
        String resourcesFingerprint = ConfigurationValidationCache.fingerprintResources(cacheResourceDirs, cacheIgnore);

        if (cacheEnabled && handleCacheHit(cacheFile, inputsFingerprint, resourcesFingerprint, outputDir, format)) {
            return;
        }

        if (getLog().isInfoEnabled()) {
            getLog().info("Validating Micronaut configuration (" + scenarioName() + ")");
        }

        cleanupStaleReports(outputDir, format);

        ConfigurationValidationExecutor.ValidationResult result = ConfigurationValidationExecutor.validate(
            classpath,
            environments,
            suppressions,
            suppressInjectErrors,
            failOnNotPresent,
            deduceEnvironments,
            validateDependencyInjection,
            outputDir,
            format,
            project.getBasedir().toPath(),
            resolveResourceDirectories(set),
            System.err
        );

        if (cacheEnabled) {
            ConfigurationValidationCache.write(
                cacheFile,
                inputsFingerprint,
                resourcesFingerprint,
                result.hasErrors() ? ConfigurationValidationCache.LastResult.FAILURE : ConfigurationValidationCache.LastResult.SUCCESS
            );
        }

        if (result.hasErrors()) {
            String message = "Micronaut configuration is not valid. See reports in: " + result.outputDirectory();
            if (!result.dependencyInjectionErrors().isEmpty()) {
                String suppressionHint = buildDependencyInjectionSuppressionHint(result.dependencyInjectionErrors());
                if (!suppressionHint.isEmpty()) {
                    message += suppressionHint;
                }
            }
            throw new MojoFailureException(message);
        }
    }

    private String buildDependencyInjectionSuppressionHint(Set<DependencyInjectionError> dependencyInjectionErrors) {
        Set<String> suppressionCandidates = new TreeSet<>();
        for (DependencyInjectionError error : dependencyInjectionErrors) {
            if (error.rootBean() != null && !error.rootBean().isBlank()) {
                suppressionCandidates.add(error.rootBean());
            }
            if (error.bean() != null && !error.bean().isBlank()) {
                suppressionCandidates.add(error.bean());
            }
        }

        if (suppressionCandidates.isEmpty()) {
            return "";
        }

        String suppressions = suppressionCandidates.stream()
            .map(suppressionCandidate -> "        <suppressInjectError>" + suppressionCandidate + "</suppressInjectError>")
            .collect(Collectors.joining(System.lineSeparator()));

        return """

            If these dependency injection errors can be ignored, add the following to your pom.xml:
            <configurationValidation>
                <validateDependencyInjection>true</validateDependencyInjection>
                <suppressInjectErrors>
            %s
                </suppressInjectErrors>
            </configurationValidation>""".formatted(suppressions);
    }

    private ConfigurationValidationFormat parseFormat(String format) throws MojoFailureException {
        try {
            return ConfigurationValidationFormat.parse(format);
        } catch (IllegalArgumentException e) {
            throw new MojoFailureException(e.getMessage());
        }
    }

    private boolean handleCacheHit(Path cacheFile,
                                   String inputsFingerprint,
                                   String resourcesFingerprint,
                                   Path outputDir,
                                   ConfigurationValidationFormat format) throws MojoFailureException {
        ConfigurationValidationCache.CacheEntry entry = ConfigurationValidationCache.readIfUpToDate(cacheFile, inputsFingerprint, resourcesFingerprint);
        if (entry == null) {
            return false;
        }
        if (entry.lastResult() == ConfigurationValidationCache.LastResult.FAILURE) {
            throw cachedFailure(outputDir, format);
        }
        if (getLog().isDebugEnabled()) {
            getLog().debug("Skipping configuration validation (cache hit) for scenario: " + scenarioName());
        }
        return true;
    }

    private void cleanupStaleReports(Path outputDir, ConfigurationValidationFormat format) {
        // Remove reports that won't be generated in the current format to avoid pointing users to stale files
        Path html = outputDir.resolve("configuration-errors.html");
        Path json = outputDir.resolve("configuration-errors.json");
        
        try {
            if (format != ConfigurationValidationFormat.HTML && format != ConfigurationValidationFormat.BOTH) {
                Files.deleteIfExists(html);
            }
            if (format != ConfigurationValidationFormat.JSON && format != ConfigurationValidationFormat.BOTH) {
                Files.deleteIfExists(json);
            }
        } catch (IOException e) {
            // Best effort cleanup; log at debug level if needed
            if (getLog().isDebugEnabled()) {
                getLog().debug("Failed to clean up stale reports", e);
            }
        }
    }

    private MojoFailureException cachedFailure(Path outputDir, ConfigurationValidationFormat format) {
        Path html = outputDir.resolve("configuration-errors.html");
        Path json = outputDir.resolve("configuration-errors.json");
        String report;
        if (format == ConfigurationValidationFormat.JSON && Files.isRegularFile(json)) {
            report = json.toAbsolutePath().normalize().toUri().toString();
        } else if (format == ConfigurationValidationFormat.HTML && Files.isRegularFile(html)) {
            report = html.toAbsolutePath().normalize().toUri().toString();
        } else if (Files.isRegularFile(html)) {
            report = html.toAbsolutePath().normalize().toUri().toString();
        } else if (Files.isRegularFile(json)) {
            report = json.toAbsolutePath().normalize().toUri().toString();
        } else {
            report = outputDir.toAbsolutePath().normalize().toUri().toString();
        }
        return new MojoFailureException("Micronaut configuration is not valid (cached). Report: " + report);
    }

    private boolean isEnabled(ConfigurationValidationConfiguration cfg) {
        return cfg.getEnabled() == null || cfg.getEnabled();
    }

    private Path determineOutputDir(ConfigurationValidationConfiguration cfg, ConfigurationValidationConfiguration.ValidationSet set) {
        File base = cfg.getOutputDirectory();
        if (base == null) {
            base = new File(project.getBuild().getDirectory(), "micronaut/config-validation");
        }
        File output;
        if (set != null && set.getOutputDirectory() != null) {
            output = set.getOutputDirectory();
        } else {
            output = new File(base, scenarioName());
        }
        return output.toPath();
    }

    private List<String> computeEnvironments(ConfigurationValidationConfiguration.ValidationSet set, List<String> defaults) {
        List<String> configured = set == null || set.getEnvironments() == null ? List.of() : set.getEnvironments();
        boolean includeDefault = set == null || set.getIncludeDefaultEnvironment() == null || set.getIncludeDefaultEnvironment();

        List<String> result = new ArrayList<>();
        if (includeDefault) {
            result.addAll(defaults);
        }
        if (configured != null) {
            for (String env : configured) {
                if (env == null || env.isBlank()) {
                    continue;
                }
                if (!result.contains(env)) {
                    result.add(env);
                }
            }
        }
        return List.copyOf(result);
    }

    private List<String> computeClasspathElements(ConfigurationValidationConfiguration.ValidationSet set) {
        Set<String> elements = new LinkedHashSet<>();

        addNonBlankElements(elements, chooseBaseClasspath(set));
        addNonBlankElements(elements, set == null ? null : set.getAdditionalClasspathElements());

        addDependencyClasspath(elements);

        return new ArrayList<>(elements);
    }

    private List<String> chooseBaseClasspath(ConfigurationValidationConfiguration.ValidationSet set) {
        if (set != null && set.getClasspathElements() != null && !set.getClasspathElements().isEmpty()) {
            return set.getClasspathElements();
        }
        return defaultClasspath(project);
    }

    private void addNonBlankElements(Set<String> elements, List<String> candidates) {
        if (candidates == null) {
            return;
        }
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                elements.add(candidate);
            }
        }
    }

    private void addDependencyClasspath(Set<String> elements) {
        List<Dependency> deps = compilerService.resolveDependencies(project, false, dependencyScopes());
        String depsClasspath = compilerService.buildClasspath(deps);
        if (depsClasspath == null || depsClasspath.isBlank()) {
            return;
        }
        Collections.addAll(elements, depsClasspath.split(java.util.regex.Pattern.quote(File.pathSeparator)));
    }

    private List<Path> computeCacheResourceDirectories(ConfigurationValidationConfiguration.ValidationSet set) {
        if (set != null && set.getResourceDirectories() != null && !set.getResourceDirectories().isEmpty()) {
            return resolvedPaths(set.getResourceDirectories().stream().filter(Objects::nonNull).map(File::toPath).toList(), false);
        }
        List<Path> defaults = defaultResourceDirectories();
        if (defaults == null || defaults.isEmpty()) {
            return List.of();
        }
        return resolvedPaths(defaults.stream().filter(Objects::nonNull).toList(), false);
    }

    private List<Path> resolveResourceDirectories(ConfigurationValidationConfiguration.ValidationSet set) {
        if (set != null && set.getResourceDirectories() != null && !set.getResourceDirectories().isEmpty()) {
            return existingPathsFromFiles(set.getResourceDirectories());
        }
        List<Path> defaults = defaultResourceDirectories();
        if (defaults == null || defaults.isEmpty()) {
            return List.of();
        }

        return existingPathsFromPaths(defaults);
    }

    private List<Path> existingPathsFromFiles(List<File> files) {
        return existingPaths(files.stream().filter(Objects::nonNull).map(File::toPath).toList());
    }

    private List<Path> existingPathsFromPaths(List<Path> paths) {
        return existingPaths(paths.stream().filter(Objects::nonNull).toList());
    }

    private List<Path> existingPaths(List<Path> paths) {
        return resolvedPaths(paths, true);
    }

    private List<Path> resolvedPaths(List<Path> paths, boolean existingOnly) {
        if (paths.isEmpty()) {
            return List.of();
        }
        List<Path> result = new ArrayList<>(paths.size());
        Path baseDir = project.getBasedir().toPath();
        for (Path p : paths) {
            Path resolved = p.isAbsolute() ? p : baseDir.resolve(p);
            resolved = resolved.normalize();
            if (!existingOnly || Files.exists(resolved)) {
                result.add(resolved);
            }
        }
        return List.copyOf(result);
    }
}
