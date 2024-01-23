File log = new File(basedir, 'build.log')
String osArch = System.getProperty("os.arch")

assert log.exists()
assert log.text.contains("mn:${pluginVersion}:graalvm-resources")
if ("aarch64".equals(osArch)) {
    assert log.text.contains("Using BASE_IMAGE_RUN: cgr.dev/chainguard/wolfi-base:latest")
} else {
    assert log.text.contains("Using BASE_IMAGE_RUN: frolvlad/alpine-glibc:glibc-2.34")
}
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-native:0.1")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")