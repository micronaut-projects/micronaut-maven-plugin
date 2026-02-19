# OPENAPI SUBDOMAIN

## OVERVIEW
OpenAPI code generation goals (`server`, `client`, `generic`) plus shared parameter/response mapping helpers.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Shared option handling | `AbstractOpenApiMojo.java` | Main configuration surface; many parameters |
| Server generator behavior | `OpenApiServerMojo.java` | Server-focused builder wiring |
| Client generator behavior | `OpenApiClientMojo.java` | Client-focused builder wiring |
| Generic generator behavior | `OpenApiGenericMojo.java` | Generic mode |
| Type/request mapping knobs | `ParameterMapping.java`, `ResponseBodyMapping.java` | Mapping models used by mojo config |

## CONVENTIONS
- New OpenAPI flags should first be modeled in `AbstractOpenApiMojo`, then consumed in specialized mojos only where needed.
- Keep mapping objects small and data-focused; generator wiring belongs in mojo classes.
- Keep option naming aligned with existing `micronaut.openapi.*` property style.

## ANTI-PATTERNS
- Do not duplicate shared option parsing in each concrete mojo.
- Do not mix OpenAPI-specific mapping logic into unrelated packages.

## UNIQUE STYLES
- OpenAPI goals share one large option surface and specialize through builder choices.
- Integration behavior is verified through `src/it/openapi-*` scenario projects.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-plugin -am -Dtest=OpenApi* test
./mvnw -pl micronaut-maven-integration-tests -am verify "-Dinvoker.test=openapi*"
```

## NOTES
- If changing default generator flags, verify both client and server scenarios to avoid asymmetric regressions.
