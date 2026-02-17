# PROJECT KNOWLEDGE BASE

**Generated:** 2026-02-17 13:31:24 CET
**Commit:** 520ced668
**Branch:** 5.0.x

## OVERVIEW
Micronaut Maven Plugin monorepo. Core work happens in the Maven plugin module plus shared core, Jib integration, enforcer rules, and a large invoker-based integration-test harness.

## STRUCTURE
```text
./
|- micronaut-maven-plugin/            # main Maven goals and feature packages
|- micronaut-maven-core/              # shared runtime/build abstractions
|- micronaut-maven-jib-integration/   # Jib extension and configuration glue
|- micronaut-maven-enforcer-rules/    # custom Maven Enforcer rule(s)
|- micronaut-maven-integration-tests/ # src/it invoker scenarios
|- examples/                          # language and multi-project samples
|- config/                            # checkstyle + spotless config
`- .github/workflows/                 # snapshot, windows, release CI
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Add/modify plugin goal behavior | `micronaut-maven-plugin/src/main/java/io/micronaut/maven` | Includes top-level mojos and feature packages |
| OpenAPI generation changes | `micronaut-maven-plugin/src/main/java/io/micronaut/maven/openapi` | Client/server/generic generators |
| AOT integration changes | `micronaut-maven-plugin/src/main/java/io/micronaut/maven/aot` | Analysis + sample config generation |
| Test resources lifecycle | `micronaut-maven-plugin/src/main/java/io/micronaut/maven/testresources` | Start/stop lifecycle + helper |
| Shared compile/dependency logic | `micronaut-maven-plugin/src/main/java/io/micronaut/maven/services` | Used by heavy mojos like `RunMojo` |
| Cross-module integration behavior | `micronaut-maven-integration-tests/src/it` | Scenario-per-directory invoker tests |
| Formatting and style rules | `config/checkstyle`, `config/spotless.license.java` | Enforced in compile phase |

## CODE MAP
| Symbol | Type | Location | Refs | Role |
|--------|------|----------|------|------|
| `RunMojo` | class | `micronaut-maven-plugin/.../RunMojo.java` | high | Main run/watch/recompile orchestrator |
| `AbstractOpenApiMojo` | class | `micronaut-maven-plugin/.../openapi/AbstractOpenApiMojo.java` | medium | Base for OpenAPI generators |
| `AotAnalysisMojo` | class | `micronaut-maven-plugin/.../aot/AotAnalysisMojo.java` | medium | AOT analysis goal |
| `TestResourcesHelper` | class | `micronaut-maven-plugin/.../testresources/TestResourcesHelper.java` | high | Test resources server/process management |
| `JibMicronautExtension` | class | `micronaut-maven-jib-integration/.../JibMicronautExtension.java` | medium | Jib build-plan integration |
| `MicronautRuntime` | enum | `micronaut-maven-core/.../MicronautRuntime.java` | medium | Runtime/build-strategy selector |

## CONVENTIONS
- Build gates run at compile/verify: Spotless + Checkstyle in compile, invoker integration tests in verify.
- Root POM owns shared plugin and quality configuration; module POMs stay lean.
- Feature packages under `io.micronaut.maven` are explicit (`openapi`, `aot`, `testresources`, `services`, `jsonschema`).
- Integration tests are isolated scenarios under `src/it/<scenario>` with per-scenario `invoker.properties` and optional `verify.groovy`.
- CI matrix is intentional: snapshot on Java 25, windows on Java 25, release workflow handles tagging + publish.

## ANTI-PATTERNS (THIS PROJECT)
- Do not add new plugin behavior only in examples; behavior must live in real modules and be covered by `src/it` scenarios.
- Do not bypass style gates (`spotless`, `checkstyle`) in normal contributor flows.
- Do not implement cross-module policy checks outside `micronaut-maven-enforcer-rules`.
- Avoid adding logic to generated `target/` content; edit sources only.
- Avoid broad changes in `RunMojo` without matching integration-test coverage.

## UNIQUE STYLES
- Large invoker test matrix is first-class; many issue/regression scenarios are encoded as mini projects.
- OpenAPI, AOT, and test-resources are treated as separate plugin sub-domains, not utility classes mixed together.
- Release workflow performs docs publish and Maven release sequence from GitHub Actions.

## COMMANDS
```bash
./mvnw clean verify
./mvnw verify "-Dinvoker.test=dockerfile*"
./mvnw -pl micronaut-maven-integration-tests -am verify
./mvnw spotless:check checkstyle:check
./mvnw -pl micronaut-maven-plugin -am test
./mvnw release:prepare -DdryRun=true
```

## NOTES
- If touching CI expectations, update `.github/workflows/snapshot.yml`, `.github/workflows/windows-ci.yml`, and/or `.github/workflows/release.yml` consistently.
- Keep child AGENTS.md files scoped: local rules only, no repeated root-level guidance.
