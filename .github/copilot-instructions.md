# GitHub Copilot Instructions for Micronaut Maven Plugin

This repository contains the Micronaut Maven Plugin, a Maven plugin to execute and manage Micronaut applications.

## Project Structure

This is a multi-module Maven project with the following modules:
- **micronaut-maven-core**: Core functionality shared across modules
- **micronaut-maven-enforcer-rules**: Maven enforcer rules for Micronaut projects
- **micronaut-maven-jib-integration**: Integration with Jib for container image building
- **micronaut-maven-plugin**: The main Maven plugin with goals and mojos
- **micronaut-maven-integration-tests**: Integration tests using Maven Invoker

## Technology Stack

- **Java**: JDK 17 (source/target compatibility)
- **Maven**: 3.9.11 minimum
- **Micronaut**: 4.9.x (managed via parent POM properties)
- **Build Tool**: Maven with Spotless for formatting and Checkstyle for code quality

## Build Commands

```bash
# Fast local build (skip integration tests)
mvn -V -ntp -Dinvoker.skip=true clean verify

# Full build with integration tests
mvn -V -ntp clean verify

# Run specific integration tests
mvn -V -ntp verify "-Dinvoker.test=pattern"

# Apply code formatting
mvn -q -ntp spotless:apply

# Generate site documentation
mvn -V -ntp site
```

## Code Style and Standards

### Formatting
- **Spotless** enforces code formatting and license headers automatically
- License header template: `config/spotless.license.java`
- All new files must include Apache 2.0 license header
- Run `mvn -q -ntp spotless:apply` before committing

### Code Quality
- **Checkstyle** is enforced with `config/checkstyle/checkstyle.xml`
- Build fails on Checkstyle violations
- No ad-hoc suppressions - use configured suppressions files

### Dependencies
- Prefer version properties from parent POM (e.g., `micronaut.version`, `maven.version`)
- Add new dependencies to `dependencyManagement` when shared across modules
- Avoid hardcoding versions in child POMs

## Maven Plugin Development Guidelines

### Mojo Development
- Extend `AbstractMicronautMojo` or appropriate base classes (e.g., `AbstractTestResourcesMojo`)
- Use `@Inject` constructor injection for services (`CompilerService`, `ExecutorService`, etc.)
- Use `getLog()` for logging - no `System.out`/`System.err` or `System.exit`
- Use proper Maven plugin annotations from `maven-plugin-annotations`

### Plugin Annotations
```java
@Mojo(name = "goal-name", 
      defaultPhase = LifecyclePhase.PACKAGE,
      requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME)
```

### Error Handling
- Throw `MojoExecutionException` for user-facing execution failures
- Log stack traces at debug level
- Provide clear, actionable error messages

### Process Management
- Use `inheritIO` for process spawning
- Ensure proper cleanup in shutdown hooks
- Always call `destroy()` and `waitFor()` with fallback to `destroyForcibly()`

## File and Path Handling

### Default Exclusions
Respect these exclusions when processing files:
- `target/**`, `**/target/**`
- `.idea/**`
- `src/test/**` (for file watching)

### Path Utilities
- Use `org.codehaus.plexus.util.AbstractScanner.match` for glob matching
- Normalize paths with forward slashes
- Use `NOFOLLOW_LINKS` to avoid following symlinks

## Testing Guidelines

### Unit Tests
- Use JUnit Jupiter 5
- Use Mockito for mocking
- junit-pioneer is available (mind existing exclusions)

### Integration Tests
- Use Maven Invoker with projects under `micronaut-maven-integration-tests/src/it/<name>`
- Provide `invoker.properties` and `verify.groovy`/`selector.groovy` files
- Test with `-Dinvoker.test=pattern` for targeted testing

### Debugging
1. Install plugin: `mvn install -Dinvoker.skip=true`
2. Use example project in `examples/java`
3. Set property `<micronaut-maven-plugin.version>` to local snapshot
4. Use `mvnDebug` and attach debugger on port 8000

## Documentation

### Site Documentation
- Primary docs in `micronaut-maven-plugin/src/site/asciidoc/`
- Site configuration: `micronaut-maven-plugin/src/site/site.xml`
- Update site menu when adding/removing pages
- Generate with: `mvn -V -ntp -Dinvoker.skip=true -pl micronaut-maven-plugin -am site`

### JavaDoc
- Public mojos and user-facing classes need JavaDoc with `@since` and `@author` tags
- Validate JavaDoc: `mvn -V -ntp -Prelease -DskipTests javadoc:jar`

### Examples
- Add new examples to `src/site/asciidoc/examples/*.adoc`
- Update `site.xml` menu accordingly
- Avoid hardcoding version numbers

## Files to Avoid Modifying

- Generated/build outputs: `target/**`, `**/target/**`
- Develocity settings: `.mvn/develocity.xml`
- GitHub Actions workflows: `.github/workflows/*`
- Checkstyle configs: `config/checkstyle/*`

## Commit Checklist

Before submitting code:
1. Apply formatting: `mvn -q -ntp spotless:apply`
2. Run checks: `mvn -V -ntp -Dinvoker.skip=true clean verify`
3. Run relevant integration tests: `mvn -V -ntp verify "-Dinvoker.test=pattern"`
4. Ensure license headers on new files
5. Update documentation if user-facing behavior changes
6. Validate JavaDoc if touching public APIs

## Common Utilities

### Helper Classes
- Use `MojoUtils` helpers (e.g., `findJavaExecutable`, `hasMicronautMavenPlugin`)
- Use existing `ExecutorService` helpers in the codebase
- Leverage dependency resolution services for Maven context

### Maven Context
- Respect `MavenSession` and `MavenProject` context
- Handle `-pl` and submodule runs properly
- Don't assume execution from repository root

## Module-Specific Notes

### micronaut-maven-plugin
- Contains the main plugin goals and mojos
- Follow existing patterns for new goals
- Use proper lifecycle phases
- Plugin descriptor is auto-generated from annotations

### micronaut-maven-core  
- Shared utilities and services
- Keep dependencies minimal and well-managed
- Common abstractions for Maven integration

### micronaut-maven-enforcer-rules
- Custom Maven Enforcer rules
- Extend appropriate base classes
- Provide clear rule documentation

### micronaut-maven-jib-integration
- Integration with Google Jib for container images
- Handle Docker/OCI image building scenarios

### Integration Tests
- Each test should be self-contained
- Provide clear verification logic
- Test both success and failure scenarios
- Use `verify.groovy` for complex validation logic

## Important Constants and Patterns

### Package Structure
- Main packages follow `io.micronaut.maven.*` pattern
- Test resources under standard Maven layout
- Configuration files in `src/main/resources`

### Maven Properties
- Use properties like `micronaut.version`, `maven.version`
- Plugin version should be `micronaut-maven-plugin.version`
- Build directory: `${project.build.directory}`

### Logging Patterns
```java
getLog().info("Processing " + project.getName());
getLog().warn("Configuration issue detected");
getLog().error("Build failed", exception);
```