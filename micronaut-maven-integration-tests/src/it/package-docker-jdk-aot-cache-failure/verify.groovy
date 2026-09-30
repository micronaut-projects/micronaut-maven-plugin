File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

// The training mode is set, so the build does not explain a default
assert text.contains("JDK AOT cache: training mode start: the training run starts the application")
assert !text.contains("has no training mode that loads it without starting it")
assert !text.contains("micronaut.docker.jdkAotCache.trainingMode is not")

// A warm-up request that fails fails the training run, and the build
assert text.contains("[jdk-aot-cache] GET /hello: 200")
assert text.contains("JDK AOT cache training failed: [jdk-aot-cache] GET /missing answered 404")
assert !text.contains("JDK AOT cache: building the image with the cache")
assert !new File(basedir, 'target/jdk-aot-cache/app.aot').exists()

// The training image is removed
def inspect = ['docker', 'image', 'inspect', 'package-docker-jdk-aot-cache-failure-jdk-aot-training'].execute()
inspect.waitForProcessOutput(new StringBuilder(), new StringBuilder())
assert inspect.exitValue() != 0
