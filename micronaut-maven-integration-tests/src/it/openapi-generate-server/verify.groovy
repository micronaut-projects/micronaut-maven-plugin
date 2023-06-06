File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("OpenAPI Generator: java-micronaut-server (server)")
assert log.text.contains("BUILD SUCCESS")

new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/api/AbstractPetController.java").exists()
new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/model/Pet.java").exists()
new File(basedir, "target/classes/io/micronaut/openapi/api/AbstractPetController.class").exists()
new File(basedir, "target/classes/io/micronaut/openapi/model/Pet.class").exists()
