# JAVA EXAMPLE

## OVERVIEW
Single-module Java sample used as the primary local debugging target for plugin behavior.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Main app bootstrap | `src/main/java/io/micronaut/maven/examples/Application.java` | Entrypoint used by run/package goals |
| HTTP sample behavior | `src/main/java/io/micronaut/maven/examples/DemoController.java` | Basic endpoint wiring |
| Auxiliary bean wiring | `src/main/java/io/micronaut/maven/examples/greeter/Greeter.java` | Injection and bean generation path |
| Example build config | `pom.xml` | Plugin usage, compiler setup, packaging toggles |

## CONVENTIONS
- Keep this sample simple and representative of default Java project structure.
- Prefer adjusting this sample only when plugin behavior changes require user-facing examples.

## ANTI-PATTERNS
- Do not add test-only behavior that does not map to real user projects.
- Do not move compatibility checks here without adding integration tests.

## COMMANDS
```bash
./mvnw -f examples/java/pom.xml test
./mvnw -f examples/java/pom.xml package
```
