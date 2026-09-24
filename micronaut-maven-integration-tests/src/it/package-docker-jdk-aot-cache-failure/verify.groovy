File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

// A warm-up request that fails fails the training run, and the build
assert text.contains("[jdk-aot-cache] GET /hello: 200")
assert text.contains("JDK AOT cache training failed: [jdk-aot-cache] GET /missing answered 404")
assert !text.contains("JDK AOT cache: building the image with the cache")
assert !new File(basedir, 'target/jdk-aot-cache/app.aot').exists()

// The training image is removed
def inspect = ['docker', 'image', 'inspect', 'package-docker-jdk-aot-cache-failure-jdk-aot-training'].execute()
inspect.waitForProcessOutput(new StringBuilder(), new StringBuilder())
assert inspect.exitValue() != 0
