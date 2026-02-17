# AOT SUBDOMAIN

## OVERVIEW
AOT analysis/sample goals and supporting runtime/config helpers.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Main AOT analysis flow | `AotAnalysisMojo.java` | Generates effective AOT config/artifacts |
| AOT sample generation | `AotSampleMojo.java` | Sample configuration helper goal |
| Shared AOT mojo behavior | `AbstractMicronautAotMojo.java`, `AbstractMicronautAotCliMojo.java` | Common base classes |
| Runtime/mode internals | `AotRuntime.java`, `Constants.java` | Shared enums/constants |

## CONVENTIONS
- Keep config serialization and file output deterministic; this area is test-sensitive.
- Add new mode-dependent behavior through base AOT abstractions before concrete mojos.
- Keep AOT CLI interaction points isolated in `AbstractMicronautAotCliMojo`.

## ANTI-PATTERNS
- Do not scatter AOT constants across unrelated packages.
- Do not perform ad hoc filesystem writes outside existing output path patterns.

## UNIQUE STYLES
- AOT goals produce config/artifacts consumed by later packaging/runtime phases.
- AOT implementation intentionally separates shared defaults from goal-specific overrides.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-plugin -am -Dtest=*Aot* test
./mvnw -pl micronaut-maven-integration-tests -am verify "-Dinvoker.test=aot*"
```

## NOTES
- AOT behavior updates usually need at least one `src/it/aot-*` or `src/it/run-aot*` scenario update.
