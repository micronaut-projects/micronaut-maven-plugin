File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("25", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText
