File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile.${graalVmArch()}")
String expectedDockerfileText = expectedDockerfile.text.replace("17", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

static String graalVmArch() {
    String osArch = System.getProperty("os.arch")
    if ("aarch64".equals(osArch)) {
        return "aarch64"
    } else {
        return "amd64"
    }
}

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Copying /function/function.zip")