import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Replacing original artifact with shaded artifact")
assert log.text.contains("Running Micronaut AOT")

File fatJar = new File(basedir, "target/package-jar-aot-0.1.jar")
assert fatJar.exists()

JarFile jarFile = new JarFile(fatJar)

//Generated
assert jarFile.stream().anyMatch{ e -> e.name == 'io/micronaut/build/examples/generated/AOTApplicationContextConfigurer.class' }

// Resource filtering
assert jarFile.stream().noneMatch{ e -> e.name == 'application.yml' }
assert jarFile.stream().noneMatch{ e -> e.name == 'logback.xml' }