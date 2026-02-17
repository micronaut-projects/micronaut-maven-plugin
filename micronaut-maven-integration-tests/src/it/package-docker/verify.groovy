File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Using base image: eclipse-temurin:${System.getProperty("java.specification.version")}-jre")
assert log.text.contains("Built image to Docker daemon as alvarosanchez/package-docker:0.1")
