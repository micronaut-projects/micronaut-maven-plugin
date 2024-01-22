File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("17-ol9", "${System.getProperty("java.specification.version")}-ol9")

assert dockerfile.text == expectedDockerfileText