File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
def javaVersion = System.getProperty("java.specification.version")
if (javaVersion == "1.8") {
    expectedDockerfile = new File(basedir, "Dockerfile.java8")
}
assert dockerfile.text == expectedDockerfile.text