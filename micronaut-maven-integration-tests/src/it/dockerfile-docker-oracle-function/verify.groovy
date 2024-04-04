File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("eclipse-temurin:21-jre-jammy", "${System.getProperty("java.specification.version") == "21" ? "eclipse-temurin:21-jre-jammy" : "eclipse-temurin:17-jre-focal"}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("fnproject/fn-java-fdk:jre17-latest")
assert log.text.contains("Successfully tagged alvarosanchez/dockerfile-docker-oracle-function:0.1")