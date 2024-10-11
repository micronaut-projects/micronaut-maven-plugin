File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("OpenAPI Generator: java-micronaut-server (server)")
assert log.text.contains("BUILD SUCCESS")

def petApi = new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/api/PetApi.java")
assert petApi.exists()
assert new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/model/Pet.java").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/api/PetApi.class").exists()
assert new File(basedir, "target/classes/io/micronaut/openapi/model/Pet.class").exists()

assert !petApi.text.contains("import jakarta.annotation.Generated;")

def category = new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/model/Category.java")
assert category.exists()
assert category.text.contains("@Accessors(chain = true)")
assert category.text.contains("@NoArgsConstructor")
assert category.text.contains("@AllArgsConstructor")
assert category.text.contains("@Data")
