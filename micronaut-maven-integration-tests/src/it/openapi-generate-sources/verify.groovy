File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Greetings from builder = Hello, world!")
assert log.text.contains("With joy = true")
assert log.text.contains("With magic = 123")
assert log.text.contains("[INFO] OpenAPI Generator: Dummy Generator (other)")
assert log.text.contains("BUILD SUCCESS")

def petApi = new File(basedir, "target/generated-sources/openapi/src/main/java/io/micronaut/openapi/api/PetApi.java")
assert petApi.exists()
assert petApi.text.replaceAll(System.lineSeparator(), "\n").contains("""
public interface PetApi {
     String GREETING = "Hello, world!";
     boolean JOY = true;
     int MAGIC_NUMBER = 123;
}""")
assert new File(basedir, "target/classes/io/micronaut/openapi/api/PetApi.class").exists()
