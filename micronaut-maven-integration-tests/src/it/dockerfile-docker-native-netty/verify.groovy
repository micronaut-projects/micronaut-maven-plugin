File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("17", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')

assert log.exists()
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")