
return isGraalJVM()

static boolean isGraalJVM() {
    return isGraal("jvmci.Compiler", "java.vendor.version")
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