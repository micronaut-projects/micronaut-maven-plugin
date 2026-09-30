File log = new File(basedir, 'build.log')
assert log.exists()
String javaVersion = System.getProperty("java.specification.version")
String image = 'alvarosanchez/package-docker-jdk-aot-cache-load:0.1'

// The log has the two builds of invoker.properties
List<String> builds = log.text.split(/(?m)^\[INFO\] Scanning for projects\.\.\.$/).toList()
assert builds.size() == 3
String startBuild = builds[1]
String text = builds[2]

// 1. With trainingMode=start, the training run starts the application, which cannot reach its backend in the image
// build: the training fails, and no image is built
assert startBuild.contains("JDK AOT cache: training mode start: the training run starts the application")
assert startBuild.contains("JDK AOT cache: training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=/tmp/app.aot -Dmicronaut.application.training.enabled=true -Dmicronaut.application.training.mode=start")
assert startBuild.contains("Bean definition [io.micronaut.build.examples.Backend] could not be loaded")
assert startBuild.contains("Connection refused")
assert startBuild =~ /JDK AOT cache training failed: Image sha256:[0-9a-f]{64} exited with code 1/
assert !startBuild.contains("JDK AOT cache: building the image with the cache")
assert startBuild.contains("BUILD FAILURE")

// 2. By default, the training run loads the bean definitions and does not start the application
assert text.contains("JDK AOT cache: training mode load, the default: the application loads its bean definitions and exits without starting")
assert text.contains("JDK AOT cache: training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=/tmp/app.aot -Dmicronaut.application.training.enabled=true -Dmicronaut.application.training.mode=load")
assert text.contains("JDK AOT cache: the application loads its bean definitions and exits without starting (Micronaut training mode load)")
assert text =~ /Training run \(micronaut\.application\.training\.mode=load\): loaded \d+ of \d+ bean definitions/
assert !text.contains("Connection refused")
assert !text.contains("[jdk-aot-cache]")
assert !text.contains("SIGTERM")
assert new File(basedir, 'target/jdk-aot-cache/app.aot').length() > 0

// The final image pins the base image the cache was trained on, and adds the cache
assert text =~ /JDK AOT cache: using base image eclipse-temurin:${javaVersion}-jre@sha256:[0-9a-f]{64}, the one the cache was trained on/
assert text.contains("Container entrypoint set to [java, -XX:AOTCache=/app/app.aot, -XX:+UseSerialGC, -cp, @/app/jib-classpath-file, io.micronaut.build.examples.Application]")
assert text =~ /JDK AOT cache: docker\.io\/alvarosanchez\/package-docker-jdk-aot-cache-load:0\.1 has the \d+ layers of the training image and the cache layer/
assert text.contains("Built image to Docker daemon as " + image)

// The training image is removed
def inspect = ['docker', 'image', 'inspect', 'package-docker-jdk-aot-cache-load-jdk-aot-training'].execute()
inspect.waitForProcessOutput(new StringBuilder(), new StringBuilder())
assert inspect.exitValue() != 0

// With -XX:AOTMode=on, the application only starts if it can use the cache, and loads its classes and the
// Micronaut classes from it. The build starts it without the Backend bean, because the backend is not there either
assert text.contains("Picked up JDK_JAVA_OPTIONS: -XX:AOTMode=on -Xlog:class+load=info")
assert text =~ /\[class,load\] io\.micronaut\.build\.examples\.HelloController source: shared objects file/
assert text =~ /\[class,load\] io\.micronaut\.runtime\.Micronaut source: shared objects file/
assert text.contains("Startup completed")

// With the Backend bean, the image does not start where the backend is missing, as the image build is. The class of
// the bean comes from the cache all the same: the training run loaded it and did not create the bean
def run = ['docker', 'run', '--rm', '-e', 'JDK_JAVA_OPTIONS=-XX:AOTMode=on -Xlog:class+load=info', image].execute()
def output = new StringBuilder()
run.waitForProcessOutput(output, output)
assert run.exitValue() != 0
assert output.toString() =~ /\[class,load\] io\.micronaut\.build\.examples\.Backend source: shared objects file/
assert output.toString().contains("Connection refused")
