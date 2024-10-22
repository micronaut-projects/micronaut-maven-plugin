static String graalVmArch() {
    String osArch = System.getProperty("os.arch")
    if ("aarch64".equals(osArch)) {
        return "aarch64"
    } else {
        return "amd64"
    }
}

File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile.${System.getProperty("java.specification.version")}.${graalVmArch()}")

assert dockerfile.text == expectedDockerfile.text

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Copying /function/function.zip")