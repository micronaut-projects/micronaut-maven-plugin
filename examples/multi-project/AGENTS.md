# MULTI-PROJECT EXAMPLE

## OVERVIEW
Parent POM sample with two modules (`lib`, `app`) used to validate multi-module wiring with Micronaut Maven plugin behavior.

## STRUCTURE
```text
multi-project/
|- pom.xml      # parent POM with module declarations
|- lib/         # shared library module
`- app/         # Micronaut application module consuming lib
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Parent/module wiring | `pom.xml` | Defines `app` and `lib` modules |
| Shared library code | `lib/src/main/java/io/micronaut/build/examples/Greeter.java` | Upstream dependency consumed by app |
| App module entrypoint | `app/src/main/java/io/micronaut/build/examples/Application.java` | Application module using shared lib |

## CONVENTIONS
- Keep parent and child module names stable because docs and examples reference them.
- Ensure app/lib boundaries remain clear: reusable code in `lib`, executable behavior in `app`.

## ANTI-PATTERNS
- Do not collapse app and lib into a single module.
- Do not add plugin-only logic into this sample that cannot be reproduced in user projects.

## COMMANDS
```bash
./mvnw -f examples/multi-project/pom.xml test
./mvnw -f examples/multi-project/pom.xml package
```
