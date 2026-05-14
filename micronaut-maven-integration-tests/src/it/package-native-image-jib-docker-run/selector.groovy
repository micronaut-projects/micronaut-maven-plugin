return isLinux() && isGraalJVM() && hasNativeLinkerPrerequisites()

static boolean isLinux() {
    return System.getProperty("os.name").toLowerCase(Locale.ENGLISH).contains("linux")
}

static boolean isGraalJVM() {
    return isGraal("jvmci.Compiler", "java.vendor.version")
}

static boolean hasNativeLinkerPrerequisites() {
    String libzArchive = queryFileFromGcc("libz.a")
    return libzArchive != null && new File(libzArchive).isFile()
}

private static boolean isGraal(String... props) {
    for (String prop : props) {
        String vv = System.getProperty(prop)
        if (vv != null && vv.toLowerCase(Locale.ENGLISH).contains("graal")) {
            return true
        }
    }
    return false
}

private static String queryFileFromGcc(String filename) {
    try {
        Process process = new ProcessBuilder("gcc", "-print-file-name=${filename}")
            .redirectErrorStream(true)
            .start()
        process.waitFor()
        if (process.exitValue() != 0) {
            return null
        }
        String path = process.inputStream.text.trim()
        return path == filename ? null : path
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt()
        return null
    } catch (IOException e) {
        return null
    }
}
