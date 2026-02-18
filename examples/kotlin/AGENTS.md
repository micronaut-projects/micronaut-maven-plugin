# KOTLIN EXAMPLE

## OVERVIEW
Single-module Kotlin sample demonstrating plugin usage with Kotlin compiler and kapt configuration.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Main app bootstrap | `src/main/kotlin/io/micronaut/maven/examples/Application.kt` | Entrypoint for Kotlin runtime sample |
| Kotlin build pipeline | `pom.xml` | `kotlin-maven-plugin`, kapt/test-kapt, and Java compiler phase wiring |
| Test sample | `src/test/kotlin/com/example/DemoKotlinTest.kt` | Baseline test verification |

## CONVENTIONS
- Keep kapt and Kotlin compiler execution order aligned with existing `pom.xml` phases.
- Preserve `exec.mainClass` and Kotlin module naming unless sample behavior changes.

## ANTI-PATTERNS
- Do not disable kapt/test-kapt just to make example builds pass.
- Do not introduce Kotlin-only behavior that diverges from plugin defaults without matching docs/tests.

## COMMANDS
```bash
./mvnw -f examples/kotlin/pom.xml test
./mvnw -f examples/kotlin/pom.xml package
```
