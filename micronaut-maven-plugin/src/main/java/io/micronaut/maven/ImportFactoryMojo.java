/*
 * Copyright 2017-2022 original authors
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

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;
import javax.inject.Inject;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

/**
 * Import beans from project dependencies by generating factories annotated with
 * <code>@Import</code> containing the list of packages.
 *
 * @author Auke Schrijnen
 * @since 4.5.0
 */
@Mojo(name = ImportFactoryMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES, threadSafe = true, requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
public class ImportFactoryMojo extends AbstractMicronautMojo {

  /**
   * Name of the import factory mojo.
   */
  public static final String MOJO_NAME = "generate-import-factory";

  private static final String MICRONAUT_IMPORTFACTORY_PREFIX = "micronaut.importfactory";

  /**
   * Reference to the Maven project on which the plugin is invoked.
   */
  private final MavenProject project;

  /**
   * Whether to enable or disable the generation of the import factory.
   *
   * @since 4.5.0
   */
  @Parameter(property = MICRONAUT_IMPORTFACTORY_PREFIX + ".enabled", defaultValue = "false")
  private boolean enabled;

  /**
   * The output directory to which all the sources will be generated.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "${project.build.directory}/generated-sources/importfactory", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".outputDirectory", required = true)
  private File outputDirectory;

  /**
   * Add the output directory to the project as a source root in order to let the generated java
   * classes be compiled and included in the project artifact.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "true", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".addCompileSourceRoot")
  private boolean addCompileSourceRoot;

  /**
   * Regexp pattern which allows including certain dependencies.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "^.*:.*$", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".includeDependenciesFilter", required = true)
  private String includeDependenciesFilter;

  /**
   * Regexp pattern which allows excluding certain dependencies.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "^$", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".excludeDependenciesFilter", required = true)
  private String excludeDependenciesFilter;

  /**
   * Regexp pattern which allows including certain packages.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "^.*$", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".includePackagesFilter", required = true)
  private String includePackagesFilter;

  /**
   * Regexp pattern which allows excluding certain packages.
   *
   * @since 4.5.0
   */
  @Parameter(defaultValue = "^$", property = MICRONAUT_IMPORTFACTORY_PREFIX + ".excludePackagesFilter", required = true)
  private String excludePackagesFilter;

  /**
   * The package name which is used for the generated import factories. When not specified a factory
   * is generated for each package within that package in order to access package protected fields.
   *
   * @since 4.5.0
   */
  @Parameter(property = MICRONAUT_IMPORTFACTORY_PREFIX + ".targetPackage")
  private String targetPackage;

  @Inject
  public ImportFactoryMojo(MavenProject project) {
    this.project = project;
  }

  @Override
  public void execute() throws MojoExecutionException {
    if (!enabled) {
      getLog().debug(this.getClass().getSimpleName() + " is disabled");
      return;
    }

    if (addCompileSourceRoot) {
      project.addCompileSourceRoot(outputDirectory.getPath());
    }

    List<Artifact> dependencies = getFilteredDependencies();

    if (dependencies.isEmpty()) {
      getLog().warn("No matching dependencies.");
      return;
    }

    getLog().info("Found " + dependencies.size() + " matching dependencies:");
    dependencies.forEach(
        dependency -> getLog().info(getIdentifier(dependency) + " at " + dependency.getFile()));

    List<String> packages = getFilteredPackages(dependencies);

    if (packages.isEmpty()) {
      getLog().warn("No matching packages.");
      return;
    }

    getLog().info("Found " + packages.size() + " matching packages:");
    packages.forEach(getLog()::info);

    if (targetPackage == null || targetPackage.isEmpty()) {
      for (String packageName : packages) {
        try {
          generateImportFactory(packageName, Collections.singletonList(packageName));
        } catch (IOException e) {
          throw new MojoExecutionException("Error creating factory for " + packageName, e);
        }
      }
    } else {
      try {
        generateImportFactory(targetPackage, packages);
      } catch (IOException e) {
        throw new MojoExecutionException("Error creating factory for " + targetPackage, e);
      }
    }
  }

  /**
   * Get the list of matching packages from the given list of dependencies.
   *
   * @param dependencies the Maven dependencies
   * @return a list of matching packages
   * @throws MojoExecutionException when the artifacts can't be read
   */
  private List<String> getFilteredPackages(List<Artifact> dependencies)
      throws MojoExecutionException {
    List<String> packages = new ArrayList<>();
    for (Artifact dependency : dependencies) {
      packages.addAll(getPackages(dependency));
    }

    Pattern includePackages = Pattern.compile(includePackagesFilter);
    Pattern excludePackages = Pattern.compile(excludePackagesFilter);

    return packages.stream().filter(includePackages.asMatchPredicate())
        .filter(excludePackages.asMatchPredicate().negate()).sorted().toList();
  }

  /**
   * Get the list of matching dependencies.
   *
   * @return the matching dependencies
   */
  private List<Artifact> getFilteredDependencies() {
    Pattern includeDependency = Pattern.compile(includeDependenciesFilter);
    Pattern excludeDependency = Pattern.compile(excludeDependenciesFilter);

    return project.getArtifacts().stream()
        .filter(dependency -> includeDependency.matcher(getIdentifier(dependency)).matches())
        .filter(dependency -> !excludeDependency.matcher(getIdentifier(dependency)).matches())
        .toList();
  }

  /**
   * Get the packages from the given artifact.
   *
   * @param artifact the Maven artifact
   * @return the list of packages
   * @throws MojoExecutionException when the artifacts can't be read
   */
  private List<String> getPackages(Artifact artifact) throws MojoExecutionException {
    try (JarFile file = new JarFile(artifact.getFile(), false)) {
      return file.stream()
          .filter(entry -> entry.getName().endsWith(".class") && entry.getName().contains("/"))
          .map(this::getPackageName).distinct().sorted().toList();
    } catch (IOException e) {
      throw new MojoExecutionException("Unable to read " + artifact.getFile(), e);
    }
  }

  /**
   * Get the identifier for the given Maven artifact.
   *
   * @param artifact the Maven artifact
   * @return the identifier for the artifact
   */
  private String getIdentifier(Artifact artifact) {
    return artifact.getGroupId() + ":" + artifact.getArtifactId();
  }

  /**
   * Get the Java package name from the given Jar entry.
   *
   * @param entry the Jar entry
   * @return the Java package name
   */
  private String getPackageName(JarEntry entry) {
    String entryName = entry.getName();
    // remove .class from the end and change format to use periods instead of forward slashes
    return entryName.substring(0, entryName.lastIndexOf('/')).replace('/', '.');
  }

  /**
   * Generate the import factory with the given packages in the given package.
   *
   * @param packageName the package in which the factory is generated
   * @param packages    the list of the packages which are imported
   * @throws IOException when the writing of the factory class fails
   */
  private void generateImportFactory(String packageName, List<String> packages) throws IOException {
    Path factoryPath = outputDirectory.toPath().resolve(packageName.replace('.', '/'))
        .resolve("ImportFactory.java");

    List<String> code = new ArrayList<>();
    code.add("package " + packageName + ";");
    code.add("");
    code.add("import io.micronaut.context.annotation.Factory;");
    code.add("import io.micronaut.context.annotation.Import;");
    code.add("jakarta.annotation.Generated");
    code.add("");
    code.add("/** Factory which allows Micronaut to import beans from the specified packages. */");
    code.add("@Generated(\"io.micronaut.maven:micronaut-maven-plugin\")");
    code.add("@Factory");
    code.add("@Import(");
    code.add("    packages = {");
    for (String name : packages) {
      code.add("      \"" + name + "\",");
    }
    code.add("    })");
    code.add("public class ImportFactory {}\n");

    Files.createDirectories(factoryPath.getParent());
    Files.write(factoryPath, code, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING);
  }

}
