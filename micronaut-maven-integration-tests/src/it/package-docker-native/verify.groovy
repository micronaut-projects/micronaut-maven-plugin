File log = new File(basedir, 'build.log')

assert log.exists()
assert log.text.contains("mn:${pluginVersion}:graalvm-resources")
assert log.text.contains("Using BASE_IMAGE_RUN: cgr.dev/chainguard/wolfi-base:latest")
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-native:0.1")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")