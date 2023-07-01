File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("OpenAPI Generator: java-micronaut-server (server)")
assert log.text.contains("BUILD SUCCESS")

assert new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/api/MappedApi.java").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/api/MappedApi.class").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/controller/MappedController.class").exists()
