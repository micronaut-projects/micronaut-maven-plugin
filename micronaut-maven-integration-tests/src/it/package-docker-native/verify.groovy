File log = new File(basedir, 'build.log')

assert log.exists()
assert log.text.contains("native:${nativeMavenPluginVersion}:generateTestResourceConfig")
assert log.text.contains("Using BASE_IMAGE_RUN: cgr.dev/chainguard/wolfi-base@sha256:52e71f61c6afd1f8d2625cff4465d8ecee156668ca665f7e9c582d1cc914eb6a")
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-native:0.1")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")
