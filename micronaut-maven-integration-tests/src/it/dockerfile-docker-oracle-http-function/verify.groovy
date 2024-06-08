File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

String expectedDockerfileText = expectedDockerfile.text.replace("eclipse-temurin:21-jre", "eclipse-temurin:${System.getProperty("java.specification.version")}-jre")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("fnproject/fn-java-fdk:jre17-latest")
assert log.text.contains("Successfully tagged alvarosanchez/dockerfile-docker-oracle-http-function:0.1")