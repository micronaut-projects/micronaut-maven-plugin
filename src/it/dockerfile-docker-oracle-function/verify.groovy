File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
def javaVersion = System.getProperty("java.specification.version")
assert dockerfile.text == expectedDockerfile.text