File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("25", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

// By default, the training run loads the bean definitions and does not start the application, which cannot reach its
// backend in the image build
assert text.contains("JDK AOT cache: training mode load, the default: the application loads its bean definitions and exits without starting")
assert text.contains("[jdk-aot-cache] Training with JDK_JAVA_OPTIONS=-XX:AOTCacheOutput=/home/app/app.aot -Dmicronaut.application.training.enabled=true -Dmicronaut.application.training.mode=load")
assert text =~ /Training run \(micronaut\.application\.training\.mode=load\): loaded \d+ of \d+ bean definitions/
assert !text.contains("[jdk-aot-cache] Waiting up to")
assert !text.contains("[jdk-aot-cache] Stopping the application with SIGTERM")
assert text.contains("[jdk-aot-cache] Wrote the JDK AOT cache /home/app/app.aot")
assert text.contains("[alvarosanchez/dockerfile-docker-jdk-aot-cache-load:0.1]: Built image")

// With -XX:AOTMode=on, the application only starts if it can use the cache, and loads its classes and the
// Micronaut classes from it. The test starts it without the Backend bean, because the backend is not there either
assert text =~ /\[class,load\] io\.micronaut\.build\.examples\.HelloController source: shared objects file/
assert text =~ /\[class,load\] io\.micronaut\.runtime\.Micronaut source: shared objects file/
assert text.contains("io.micronaut.runtime.Micronaut - Startup completed")
