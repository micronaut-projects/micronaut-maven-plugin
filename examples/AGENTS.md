# EXAMPLES KNOWLEDGE BASE

## OVERVIEW
Consumer-facing sample projects used for manual verification and debugging flows. These are references for Java, Kotlin, Groovy, and multi-module usage.

## STRUCTURE
```text
examples/
|- java/           # single-module Java sample
|- kotlin/         # single-module Kotlin sample
|- groovy/         # single-module Groovy sample
`- multi-project/  # parent POM with app + lib modules
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Java baseline sample | `java` | Usually the most up-to-date debug target |
| Kotlin sample setup | `kotlin/pom.xml` | Kotlin plugin + kapt wiring |
| Groovy sample setup | `groovy/pom.xml` | Groovy runtime and gmavenplus wiring |
| Multi-module usage pattern | `multi-project` | Parent + child module wiring for app/lib |

## CONVENTIONS
- Keep examples runnable as standalone projects from their own directories.
- Keep plugin usage realistic: prefer default plugin configuration over test-only shortcuts.
- Use examples to demonstrate behavior, not to host plugin implementation logic.

## ANTI-PATTERNS
- Do not rely on example-only changes as proof of plugin feature correctness.
- Do not couple one example to artifacts from another example directory.

## COMMANDS
```bash
./mvnw -f examples/java/pom.xml test
./mvnw -f examples/kotlin/pom.xml test
./mvnw -f examples/groovy/pom.xml test
./mvnw -f examples/multi-project/pom.xml test
```

## NOTES
- If an example requires updates due to plugin behavior changes, add or adjust matching invoker scenarios in `micronaut-maven-integration-tests/src/it`.
