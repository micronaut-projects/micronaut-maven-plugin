File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("25", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

// The build context has the application JAR and the class path: the dependencies in Maven order, then the JAR
File classpath = new File(basedir, 'target/jdk-aot-cache/classpath')
assert new File(basedir, 'target/jdk-aot-cache/application.jar').length() > 0
assert classpath.text.startsWith('"/home/app/libs/release/')
assert classpath.text.trim().endsWith(':/home/app/application.jar"')
assert !classpath.text.contains('*')

File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

// Micronaut 5.0 has no training mode that does not start the application, so the default training run starts it,
// and the build says why
assert text.contains("JDK AOT cache: training mode start: the training run starts the application, because its Micronaut version has no training mode that loads it without starting it (micronaut.application.training.mode).")

// Training paths without a training mode: this build fails once the application's Micronaut version has the load
// mode, and the build warns about it
assert text.contains("[WARNING] JDK AOT cache: micronaut.docker.jdkAotCache.trainingPaths is set and micronaut.docker.jdkAotCache.trainingMode is not. This build will fail once the application uses a Micronaut version with the load training mode")

// Micronaut 5.0 has no training-run switch either: the script warms the application up and stops it with SIGTERM
assert text.contains("[jdk-aot-cache] Training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=/home/app/app.aot -XX:-UsePerfData")
assert text.contains("[jdk-aot-cache] GET /hello: 200")
assert text.contains("[jdk-aot-cache] Stopping the application with SIGTERM")
assert text.contains("[jdk-aot-cache] Wrote the JDK AOT cache /home/app/app.aot")
assert text.contains("[alvarosanchez/dockerfile-docker-jdk-aot-cache:0.1]: Built image")

// With -XX:AOTMode=on, the application only starts if it can use the cache, and loads its classes and the
// Micronaut classes from it
assert text =~ /\[class,load\] io\.micronaut\.build\.examples\.HelloController source: shared objects file/
assert text =~ /\[class,load\] io\.micronaut\.runtime\.Micronaut source: shared objects file/
assert text.contains("io.micronaut.runtime.Micronaut - Startup completed")

// No JVM of the training writes a performance data file (-XX:-UsePerfData), so the layer of the RUN instruction has
// no /tmp/hsperfdata_<user>/<pid> file
def find = ['docker', 'run', '--rm', '--entrypoint', 'find', 'alvarosanchez/dockerfile-docker-jdk-aot-cache:0.1', '/tmp', '-path', '/tmp/hsperfdata_*/*'].execute()
def found = new StringBuilder()
find.waitForProcessOutput(found, found)
assert find.exitValue() == 0
assert found.toString().isEmpty()
