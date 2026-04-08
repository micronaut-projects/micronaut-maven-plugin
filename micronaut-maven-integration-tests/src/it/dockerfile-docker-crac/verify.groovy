File checkpointDockerfile = new File("$basedir/target", "Dockerfile.crac.checkpoint")
File expectedCheckpointDockerfile = new File(basedir, "Dockerfile.crac.checkpoint")

assert checkpointDockerfile.text == expectedCheckpointDockerfile.text
assert !checkpointDockerfile.text.contains("api.azul.com/metadata")
assert !checkpointDockerfile.text.contains("latest=true")

File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

assert dockerfile.text == expectedDockerfile.text
