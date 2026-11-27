import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Precompiled logback.xml")

String configurator = 'io/micronaut/aot/logback/generated/LogbackConfigurator.class'
String service = 'META-INF/services/ch.qos.logback.classic.spi.Configurator'
File fatJar = new File(basedir, "target/package-jar-logback-precompile-0.1.jar")
assert fatJar.exists()
new JarFile(fatJar).withCloseable { jar ->
    assert jar.getEntry(configurator) != null
    assert jar.getEntry('logback.xml') != null
    assert jar.getInputStream(jar.getEntry(service)).text.readLines()*.trim().findAll() ==
            ['io.micronaut.aot.logback.generated.LogbackConfigurator']
}
assert new File(basedir, "target/micronaut-logback/precompiled.txt").exists()

// The tests ran before the goal, on Joran
assert log.text.contains("Tests run: 1, Failures: 0")

