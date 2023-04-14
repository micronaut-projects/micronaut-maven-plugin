return isX86()

static boolean isX86() {
    String osArch = System.getProperty("os.arch")
    return osArch in ['amd64', 'x86_64']
}