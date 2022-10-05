File checkpointDockerfile = new File("$basedir/target", "Dockerfile.crac.checkpoint")
File expectedCheckpointDockerfile = new File(basedir, "Dockerfile.crac.checkpoint")

assert checkpointDockerfile.text == expectedCheckpointDockerfile.text

File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")

assert dockerfile.text == expectedDockerfile.text