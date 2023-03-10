return isX86()

static boolean isX86() {
    String osArch = System.getProperty("os.arch")
    println "os.arch: " + osArch
    return "x86_64" == osArch
}