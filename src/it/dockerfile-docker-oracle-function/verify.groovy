File dockerfile = new File("$basedir/target", "Dockerfile")
def javaVersion = System.getProperty("java.specification.version")
def majorJavaVersion = javaVersion
if (javaVersion == "1.8") {
    majorJavaVersion = "8"
}
File expectedDockerfile = new File(basedir, "Dockerfile.java${majorJavaVersion}")
assert dockerfile.text == expectedDockerfile.text