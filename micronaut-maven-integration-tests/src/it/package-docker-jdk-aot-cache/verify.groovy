File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
String javaVersion = System.getProperty("java.specification.version")

// The training image is built for the Docker daemon's platform only, in Jib's packaged mode
assert text.contains("JDK AOT cache: building the training image package-docker-jdk-aot-cache-jdk-aot-training")
assert text =~ /JDK AOT cache: building for the Docker daemon's platform linux\/(amd64|arm64) only/
assert text.contains("Container entrypoint set to [java, -XX:+UseSerialGC, -cp, @/app/jib-classpath-file, io.micronaut.build.examples.Application]")

// Micronaut 5.0 has no training-run switch: the plugin warms the application up and stops it with SIGTERM
assert text.contains("JDK AOT cache: training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=/tmp/app.aot")
assert text.contains("[jdk-aot-cache] GET /hello: 200")
assert text.contains("JDK AOT cache: stopping the application with SIGTERM")
assert new File(basedir, 'target/jdk-aot-cache/app.aot').length() > 0

// The final image pins the base image the cache was trained on, and adds the cache
assert text =~ /JDK AOT cache: using base image eclipse-temurin:${javaVersion}-jre@sha256:[0-9a-f]{64}, the one the cache was trained on/
assert text.contains("Container entrypoint set to [java, -XX:AOTCache=/app/app.aot, -XX:+UseSerialGC, -cp, @/app/jib-classpath-file, io.micronaut.build.examples.Application]")
assert text =~ /JDK AOT cache: docker\.io\/alvarosanchez\/package-docker-jdk-aot-cache:0\.1 has the \d+ layers of the training image and the cache layer/
assert text.contains("Built image to Docker daemon as alvarosanchez/package-docker-jdk-aot-cache:0.1")

// The training image is removed
def inspect = ['docker', 'image', 'inspect', 'package-docker-jdk-aot-cache-jdk-aot-training'].execute()
inspect.waitForProcessOutput(new StringBuilder(), new StringBuilder())
assert inspect.exitValue() != 0

// With -XX:AOTMode=on, the application only starts if it can use the cache, and loads its classes and the
// Micronaut classes from it
assert text.contains("Picked up JDK_JAVA_OPTIONS: -XX:AOTMode=on -Xlog:class+load=info")
assert text =~ /\[class,load\] io\.micronaut\.build\.examples\.HelloController source: shared objects file/
assert text =~ /\[class,load\] io\.micronaut\.runtime\.Micronaut source: shared objects file/
assert text.contains("Startup completed")
