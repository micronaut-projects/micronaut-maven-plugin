File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("17", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("public.ecr.aws/lambda/java:${System.getProperty("java.specification.version")}")
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-lambda:0.1")