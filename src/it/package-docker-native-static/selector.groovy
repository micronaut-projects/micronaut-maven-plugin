return isX86()

static boolean isX86() {
    String osArch = System.getProperty("os.arch")
    return "x86_64" == osArch
}