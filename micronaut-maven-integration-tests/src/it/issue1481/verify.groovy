File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Maven build failed"
assert log.text.contains("Built image to Docker daemon") : "Docker build failed"
assert log.text.contains("Start")
assert log.text.contains("Stop")
