File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("OpenAPI Generator: kotlin-micronaut-client (client)")
assert log.text.contains("BUILD SUCCESS")

def petApi = new File(basedir, "target/generated-sources/openapi/src/main/kotlin/io/micronaut/openapi/api/PetApi.kt")
assert petApi.exists()
assert new File(basedir, "target/generated-sources/openapi/src/main/kotlin/io/micronaut/openapi/model/Pet.kt").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/api/PetApi.class").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/model/Pet.class").exists()

assert !petApi.text.contains("import jakarta.annotation.Generated;")
