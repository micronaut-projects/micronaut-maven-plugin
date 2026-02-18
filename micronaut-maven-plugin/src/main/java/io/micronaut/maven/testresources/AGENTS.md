# TESTRESOURCES SUBDOMAIN

## OVERVIEW
Test-resources server lifecycle management for Maven builds and run-mode workflows.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Start/stop goals | `StartTestResourcesServerMojo.java`, `StopTestResourcesServerMojo.java` | User-facing lifecycle goals |
| Shared goal behavior | `AbstractTestResourcesMojo.java` | Base wiring |
| Process/server lifecycle internals | `TestResourcesHelper.java` | Main orchestration class |
| Lifecycle integration | `TestResourcesLifecycleExtension.java` | Maven lifecycle extension logic |
| Settings and factory details | `TestResourcesConfiguration.java`, `DefaultServerFactory.java` | Config and server creation |

## CONVENTIONS
- Keep lifecycle state transitions explicit (start/connect/stop) and centralize in helper/factory classes.
- Preserve compatibility with both explicit and implicit server-start flows.
- Keep environment/system-property wiring in one place to avoid mismatch between start and run flows.

## ANTI-PATTERNS
- Do not duplicate server property computation across goals.
- Do not add hidden side effects to lifecycle extension hooks.

## UNIQUE STYLES
- Lifecycle extension behavior is coupled to Maven session behavior and must remain deterministic.
- Shared namespace and keep-alive semantics are tested through dedicated `src/it/test-resources-*` scenarios.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-plugin -am -Dtest=*TestResources* test
./mvnw -pl micronaut-maven-integration-tests -am verify "-Dinvoker.test=test-resources*"
```

## NOTES
- For server lifecycle changes, validate start/stop plus shared-namespace scenarios together.
