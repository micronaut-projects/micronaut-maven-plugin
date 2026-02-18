# GROOVY EXAMPLE

## OVERVIEW
Single-module Groovy sample demonstrating plugin usage with Micronaut Groovy runtime and gmavenplus integration.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Main app bootstrap | `src/main/groovy/io/micronaut/maven/examples/Application.groovy` | Entrypoint for Groovy runtime sample |
| Groovy build setup | `pom.xml` | `gmavenplus-plugin` and Groovy runtime dependency wiring |
| Test sample | `src/test/groovy/com/example/DemoTest.groovy` | Baseline Groovy test path |

## CONVENTIONS
- Keep Groovy runtime and plugin wiring in sync with current Micronaut parent settings.
- Prefer minimal sample code; this project documents setup patterns, not framework experiments.

## ANTI-PATTERNS
- Do not add plugin behavior checks only here without invoker coverage.
- Do not depend on generated files under `target/` for source changes.

## COMMANDS
```bash
./mvnw -f examples/groovy/pom.xml test
./mvnw -f examples/groovy/pom.xml package
```
