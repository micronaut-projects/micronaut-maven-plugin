File log = new File(basedir, 'build.log')
assert log.exists()

String image = System.getProperty("java.specification.version") == "21" ? "eclipse-temurin:21-jre-jammy" : "eclipse-temurin:17-jre-focal"

assert log.text.contains("Using base image: ${image}")
assert log.text.contains("Built image to Docker daemon as alvarosanchez/package-docker:0.1")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")