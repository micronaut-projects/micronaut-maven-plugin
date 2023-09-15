// Configures and expects and ARM build on CI (via system property)
File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

assert dockerfile.text == expectedDockerfile.text
