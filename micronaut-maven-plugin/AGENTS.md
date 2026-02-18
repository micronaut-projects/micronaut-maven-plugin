# MODULE KNOWLEDGE BASE

## OVERVIEW
Main Maven plugin module (`packaging: maven-plugin`). This is where user-facing `mn:*` goals are implemented.

## STRUCTURE
```text
micronaut-maven-plugin/
|- src/main/java/io/micronaut/maven/
|  |- openapi/        # OpenAPI generation goals + mappings
|  |- aot/            # AOT analysis/sample goals
|  |- testresources/  # test-resources lifecycle goals/helpers
|  |- services/       # internal compile/exec/dependency services
|  `- *.java          # top-level mojos (run, docker*, import-factory, etc.)
`- src/test/java/     # unit tests for mojos/helpers
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Runtime/watch behavior | `src/main/java/io/micronaut/maven/RunMojo.java` | Largest and highest-impact orchestrator |
| Docker packaging behavior | `src/main/java/io/micronaut/maven/Docker*Mojo.java` | Docker, native, crac, push, Dockerfile |
| OpenAPI generation flags | `src/main/java/io/micronaut/maven/openapi` | Shared base class + 3 goal variants |
| Test resources server lifecycle | `src/main/java/io/micronaut/maven/testresources` | Start/stop goals and helper internals |
| Internal execution flow | `src/main/java/io/micronaut/maven/services` | Reused by heavy mojos |

## CONVENTIONS
- Keep new goal logic in dedicated feature package when domain-specific; avoid adding unrelated logic to top-level mojos.
- Reuse `services/*` for compile/dependency/exec mechanics instead of duplicating command orchestration.
- For new plugin behavior, prefer adding/adjusting unit tests in this module and integration tests in `micronaut-maven-integration-tests`.

## ANTI-PATTERNS
- Do not add complex flow control directly to tiny utility classes; keep orchestration in mojos/services.
- Do not modify generated plugin descriptor output under `target/generated-sources`.
- Do not add behavior that bypasses existing dependency/toolchain resolution services.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-plugin -am test
./mvnw -pl micronaut-maven-plugin -am verify
./mvnw -pl micronaut-maven-plugin -am -Dtest=RunMojoTest test
```
