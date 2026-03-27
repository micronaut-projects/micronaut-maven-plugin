File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
assert text.contains("localhost:65432")
assert text.contains("Cannot connect to the Docker daemon at tcp://localhost:65432. Is the docker daemon running?") ||
    (text.contains("Failed to read output of 'docker info'") && text.contains("error during connect: Get \"http://localhost:65432/"))
