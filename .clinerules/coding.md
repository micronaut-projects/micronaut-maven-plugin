# Cline Workspace Rules: Micronaut Maven Plugin

These rules guide code changes and operations in this repository to match the existing build, style, and release conventions.

Repository: micronaut-projects/micronaut-maven-plugin  
Modules:
- micronaut-maven-core
- micronaut-maven-enforcer-rules
- micronaut-maven-jib-integration
- micronaut-maven-plugin
- micronaut-maven-integration-tests

Tech baseline:
- Java: JDK 21 (maven.compiler.source/target = 21)
- Maven: 3.9.11
- Micronaut: 4.9.x (managed via parent POM properties)

Build and test commands
- Fast local build (skip invoker/integration tests)
    - mvn -V -ntp -Dinvoker.skip=true clean verify
- Full build (includes invoker-based integration tests)
    - mvn -V -ntp clean verify
- Run specific integration tests (Maven Invoker)
    - mvn -V -ntp verify "-Dinvoker.test=pattern"
- Apply formatting (Spotless)
    - mvn -q -ntp spotless:apply
- Generate site docs (for the plugin site)
    - mvn -V -ntp site

Formatting and style
- Spotless runs in compile phase and enforces license headers:
    - License header template: config/spotless.license.java
    - Java test sources (src/test/java) are excluded from Spotless Java format by config
    - package-info.java and module-info.java must include the license header (configured via formats)
- Checkstyle is enforced (fails build on violations):
    - Config: config/checkstyle/checkstyle.xml
    - Executed during compile (maven-checkstyle-plugin with failOnViolation=true)
- Before committing, run:
    - mvn -q -ntp spotless:apply
    - mvn -V -ntp -Dinvoker.skip=true clean verify
- New files must include the Apache 2.0 license header matching config/spotless.license.java

Maven plugin (Mojo) code conventions
- Extend AbstractMicronautMojo or the appropriate base (e.g., AbstractTestResourcesMojo) to inherit logging setup (JansiLog).
- Use getLog() for logging (no System.out/System.err). Prefer info/warn/error with concise messages.
- Prefer constructor injection with javax.inject.Inject for services (CompilerService, ExecutorService, DependencyResolutionService, etc.).
- Use helpers in MojoUtils (e.g., findJavaExecutable, hasMicronautMavenPlugin) where possible.
- Respect plugin annotations from org.apache.maven.plugins: maven-plugin-annotations (e.g., @Mojo with correct params like requiresDependencyResolution, defaultPhase, aggregator).
- Validate and respect Maven session/project context (MavenSession, MavenProject). Avoid assumptions about being run at repo root; handle -pl and submodule runs.
- For process management (e.g., run goals):
    - Avoid race conditions; follow existing patterns using AtomicBoolean and ReentrantLock for recompilation/restart gating.
    - When spawning processes, inheritIO and ensure proper cleanup in shutdown hooks; always call destroy and waitFor with fallback to destroyForcibly on interrupt.
- Avoid introducing new runtime dependencies; if needed, add versions via parent POM properties and manage via dependencyManagement.

File watching and paths
- Default exclusions to respect:
    - target/**, **/target/**, .idea/**, src/test/** (see RunMojo DEFAULT_EXCLUDES)
- Use org.codehaus.plexus.util.AbstractScanner.match for glob matching and forward-slash path normalization.
- Avoid following symlinks; use NOFOLLOW_LINKS where appropriate.

Testing guidelines
- Unit tests: JUnit Jupiter 5; use Mockito for mocking; junit-pioneer available (be mindful of exclusions present).
- Integration tests: Use Maven Invoker with example projects under micronaut-maven-integration-tests/src/it/<name>
    - Provide invoker.properties and verify.groovy/selector.groovy where applicable.
    - Run a subset with -Dinvoker.test=pattern for local iteration.
- For plugin debugging:
    - mvn install -Dinvoker.skip=true
    - In examples/java, set the property <micronaut-maven-plugin.version> to the local snapshot published, then use mvnDebug ... and attach debugger on 8000.

Dependency and versioning conventions
- Prefer version properties defined in the parent POM (e.g., micronaut.version, maven.version, jackson.version).
- Add new dependencies to dependencyManagement when shared, or scoped within a module if specific.
- Keep Micronaut and other BOM-managed libraries consistent via parent properties.
- Avoid hardcoding versions in child POMs unless strictly necessary.

Directories and files to avoid editing or committing
- Generated/build outputs: target/**, **/target/**
- Do not change Develocity/Gradle Enterprise settings unless necessary (.mvn/develocity.xml)
- Respect GitHub Actions workflow definitions (.github/workflows/*); coordinate changes with maintainers.
- Keep Checkstyle configs in config/checkstyle synchronized; do not add ad-hoc local suppressions outside the configured suppressions files.

Public API, docs, and metadata
- Public mojos and user-facing classes should have JavaDoc with @since and author tags.
- If user-facing behavior changes, update site docs under micronaut-maven-plugin/src/site/asciidoc and README if required.
- Keep README build/test instructions accurate (e.g., integration test tips, debugging guidance).

PR checklist (before submitting)
- Formatting applied: mvn -q -ntp spotless:apply
- Checks pass locally: mvn -V -ntp -Dinvoker.skip=true clean verify
- Integration tests run or scoped justification provided: mvn -V -ntp verify "-Dinvoker.test=pattern"
- License headers present on new files
- Checkstyle passes (no violations)
- Changes adhere to module boundaries and use managed versions
- Update docs/site as needed for user-facing changes

Cline operational guidance (for automated edits)
- Prefer targeted edits; keep surrounding context unchanged to minimize formatting churn.
- Preserve import ordering and let Spotless handle final formatting.
- Always add license headers to new files.
- Use getLog() instead of System.out/System.err; never call System.exit in mojos.
- Throw MojoExecutionException for user-facing execution failures; log stack traces at debug level where appropriate.
- When editing file watching logic, maintain DEFAULT_EXCLUDES and avoid watching target and test trees.
- For command execution in mojos, prefer ExecutorService helpers already present in the codebase.

Command quick reference
- Fast feedback: mvn -V -ntp -Dinvoker.skip=true clean verify
- Full verify: mvn -V -ntp clean verify
- Single invoker test: mvn -V -ntp verify "-Dinvoker.test=pattern"
- Format: mvn -q -ntp spotless:apply
- Site: mvn -V -ntp site
