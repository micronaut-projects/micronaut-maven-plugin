File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("17-ol9", "${System.getProperty("java.specification.version")}-ol9")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Successfully tagged alvarosanchez/dockerfile-docker-native-oracle-function:0.1")