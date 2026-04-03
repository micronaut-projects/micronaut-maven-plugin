File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

assert dockerfile.text == expectedDockerfile.text

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Using Dockerfile:")
