# JIB INTEGRATION MODULE

## OVERVIEW
Module for Jib extension glue used by the plugin to influence container build plans.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Extension entry point | `src/main/java/io/micronaut/maven/jib/JibMicronautExtension.java` | Main Jib extension implementation |
| Jib config modeling | `src/main/java/io/micronaut/maven/jib/JibConfiguration*.java` | Config parsing + service logic |
| Service registration | `src/main/resources/META-INF/services/...JibMavenPluginExtension` | Extension loading contract |

## CONVENTIONS
- Keep Jib-specific behavior in this module; call into `micronaut-maven-core` for shared runtime/build strategy decisions.
- Preserve deterministic build-plan transformation in extension code.
- Keep extension registration metadata and implementation class names in sync.

## ANTI-PATTERNS
- Do not hardcode platform/runtime assumptions without using existing runtime strategy helpers.
- Do not leak Jib internals into top-level plugin mojo packages.

## UNIQUE STYLES
- Extension code transforms build plans rather than shelling out to Docker CLI flows.
- Module tests validate plan mutation details instead of full container publish behavior.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-jib-integration -am test
```

## NOTES
- If Jib extension API versions change, re-check service registration and extension entry signatures first.
