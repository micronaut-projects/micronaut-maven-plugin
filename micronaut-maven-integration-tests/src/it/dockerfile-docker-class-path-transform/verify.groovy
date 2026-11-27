import java.util.jar.JarFile

File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text.replace("25", "${System.getProperty("java.specification.version")}")

assert dockerfile.text == expectedDockerfileText

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Desugared ")
assert log.text.contains("Stripped local-variable tables")
assert log.text.contains("[alvarosanchez/dockerfile-docker-class-path-transform:0.1]: Built image")
assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")

// The Docker context holds the rewritten copies of the dependencies, under their own names
File core = new File(basedir, "target/dependency/release").listFiles().find { it.name ==~ /micronaut-core-\d.*\.jar/ }
assert core != null
new JarFile(core).withCloseable { jar ->
    assert jar.entries().toList().any { it.name.contains('$$Lambda$R') }
}
