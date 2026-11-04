File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

assert text.contains("micronaut.docker.jdkAotCache runs the application in a Docker container to train the cache, so it needs a Docker daemon, also with jib.buildGoal=buildTar or build.")
assert !text.contains("Containerizing application")
assert !new File(basedir, 'target/jib-image.tar').exists()
