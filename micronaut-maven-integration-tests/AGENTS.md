# INTEGRATION TEST HARNESS

## OVERVIEW
Invoker-based integration harness. Each `src/it/<scenario>` directory is an isolated mini project executed by `maven-invoker-plugin`.

## STRUCTURE
```text
micronaut-maven-integration-tests/
|- src/it/
|  |- <scenario>/
|  |  |- pom.xml
|  |  |- invoker.properties
|  |  `- verify.groovy (optional)
|  `- openapi-setup/    # shared test fixture project
`- pom.xml             # invoker configuration + jacoco aggregate
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Harness behavior | `pom.xml` | Invoker setup (goals/hooks/selector/local repo) |
| Scenario selection | `src/it/*/invoker.properties` | Per-scenario goals and expectations |
| Extra assertions | `src/it/*/verify.groovy` | Post-build verification rules |
| OpenAPI fixture plumbing | `src/it/openapi-setup` | Dummy generator and options fixtures |

## CONVENTIONS
- Keep scenarios isolated; do not share mutable state between scenario directories.
- Use scenario names to describe behavior/regression (`issue####`, `dockerfile-*`, `test-resources-*`, etc.).
- Prefer `invoker.properties` and `verify.groovy` for scenario-specific behavior over special harness branching.

## ANTI-PATTERNS
- Do not hide new plugin behavior changes without adding/adjusting an invoker scenario.
- Do not couple scenarios through shared build artifacts outside configured invoker paths.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-integration-tests -am verify
./mvnw verify "-Dinvoker.test=dockerfile*"
./mvnw verify "-Dinvoker.test=openapi*"
./mvnw verify "-Dinvoker.test=test-resources*"
```
