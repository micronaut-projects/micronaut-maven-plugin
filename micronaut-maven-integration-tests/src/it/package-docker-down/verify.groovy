File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Cannot connect to the Docker daemon at tcp://localhost:65432. Is the docker daemon running?")