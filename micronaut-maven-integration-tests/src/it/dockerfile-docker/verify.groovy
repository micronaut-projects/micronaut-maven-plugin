File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("17", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()

String image = System.getProperty("java.specification.version") == "21" ? "eclipse-temurin:21-jre-jammy" : "eclipse-temurin:17-jre-focal"

assert log.text.contains(image)
assert log.text.contains("[alvarosanchez/dockerfile-docker:0.1]: Built image")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")