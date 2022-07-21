File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile.${graalVmArch()}")

assert dockerfile.text == expectedDockerfile.text

static String graalVmArch() {
    String osArch = System.getProperty("os.arch");
    if ("aarch64".equals(osArch)) {
        return "aarch64";
    } else {
        return "amd64";
    }
}