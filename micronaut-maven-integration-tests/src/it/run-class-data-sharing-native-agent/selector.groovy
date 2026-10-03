// The native image agent needs a GraalVM JDK, and class data sharing for mn:run needs JDK 25 or later.
return isGraalJVM() && Runtime.version().feature() >= 25

static boolean isGraalJVM() {
    for (String prop : ["jvmci.Compiler", "java.vendor.version"]) {
        String value = System.getProperty(prop)
        if (value != null && value.toLowerCase(Locale.ENGLISH).contains("graal")) {
            return true
        }
    }
    return false
}
