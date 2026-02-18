# JSONSCHEMA SUBDOMAIN

## OVERVIEW
JSON Schema code generation goal (`generate-jsonschema`) and its parameter surface.

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Goal entrypoint and parameter handling | `JsonSchemaGeneratorMojo.java` | Handles enablement, input source selection, output path, and generation call |

## CONVENTIONS
- Keep new generator parameters under the `micronaut.jsonschema.generator.*` prefix.
- Preserve deterministic output-directory layout under `generated-sources/jsonschema`.
- Keep source input precedence explicit (`input-url`, `input-file`, `input-directory`).

## ANTI-PATTERNS
- Do not add hidden fallback input selection when no input is provided.
- Do not bypass compile-source-root registration for generated sources.

## COMMANDS
```bash
./mvnw -pl micronaut-maven-plugin -am -Dtest=*JsonSchema* test
./mvnw -pl micronaut-maven-integration-tests -am verify "-Dinvoker.test=jsonschema*"
```

## NOTES
- Changes here usually require verifying both goal parameter parsing and generated-source registration behavior.
