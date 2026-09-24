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

// Micronaut 5.0 has no training-run switch: the script warms the application up and stops it with SIGTERM
assert text.contains("[jdk-aot-cache] GET /hello: 200")
assert text.contains("[jdk-aot-cache] Stopping the application with SIGTERM")
assert text.contains("[jdk-aot-cache] Wrote the JDK AOT cache /home/app/app.aot")
assert text.contains("[alvarosanchez/dockerfile-docker-jdk-aot-cache:0.1]: Built image")

// With -XX:AOTMode=on, the application only starts if it can use the cache, and loads its classes and the
// Micronaut classes from it
assert text =~ /\[class,load\] io\.micronaut\.build\.examples\.HelloController source: shared objects file/
assert text =~ /\[class,load\] io\.micronaut\.runtime\.Micronaut source: shared objects file/
assert text.contains("io.micronaut.runtime.Micronaut - Startup completed")
