File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

String expectedDockerfileText = expectedDockerfile.text.replace("eclipse-temurin:25-jre", "eclipse-temurin:${System.getProperty("java.specification.version")}-jre")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("fnproject/fn-java-fdk:jre17-1.1.7@sha256:50a0b8138fec3dde64aaedbe66406da6c2c17fc88446726c75446cb4c4fc681f")
assert log.text.contains("Successfully tagged alvarosanchez/dockerfile-docker-oracle-http-function:0.1")
