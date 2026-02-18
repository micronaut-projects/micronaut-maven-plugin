# MODULE KNOWLEDGE BASE

## OVERVIEW
Shared core abstractions used by multiple modules. This module is intentionally small and provides runtime/build-strategy enums plus shared configuration services.

## STRUCTURE
```text
micronaut-maven-core/
`- src/main/java/io/micronaut/maven/
   |- core/      # runtime and docker build strategy enums
   `- services/  # shared application configuration service
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Runtime selection behavior | `src/main/java/io/micronaut/maven/core/MicronautRuntime.java` | Canonical runtime enum used by plugin and Jib integration |
| Docker strategy selection | `src/main/java/io/micronaut/maven/core/DockerBuildStrategy.java` | Shared build-strategy enum |
| Shared app config service | `src/main/java/io/micronaut/maven/services/ApplicationConfigurationService.java` | Reused by plugin and extensions |

## CONVENTIONS
- Keep this module dependency-light and focused on shareable primitives.
- Add cross-module enums/services here only when two or more modules need the same contract.
- Preserve stable naming because downstream modules reference these types directly.

## ANTI-PATTERNS
- Do not move plugin-specific orchestration logic into this module.
- Do not introduce lifecycle behavior requiring Maven session state in core abstractions.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-core -am test
./mvnw -pl micronaut-maven-core -am verify
```

## NOTES
- If changing enums/services here, validate impacted modules: `micronaut-maven-plugin` and `micronaut-maven-jib-integration`.
