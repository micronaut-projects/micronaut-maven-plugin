File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

assert dockerfile.text == expectedDockerfile.text

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Generating a mostly static native image")
assert log.text.contains("GraalVM native image build args:")
assert log.text.contains("-H:+StaticExecutableWithDynamicLibC")