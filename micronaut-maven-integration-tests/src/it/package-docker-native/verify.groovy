File log = new File(basedir, 'build.log')

assert log.exists()
assert log.text.contains("native:${nativeMavenPluginVersion}:generateTestResourceConfig")
assert log.text.contains("Using BASE_IMAGE_RUN: cgr.dev/chainguard/wolfi-base@sha256:a5a619c1793039dcf92f02178f37c94bb3d6001403716da59d6092dfe8d9b502")
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-native:0.1")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")
