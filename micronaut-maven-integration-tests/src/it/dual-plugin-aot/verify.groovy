import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('BUILD SUCCESS')
assert log.text.contains('--- mn-aot:')
assert log.text.contains('Skipping standalone AOT analysis because micronaut-maven-plugin already owns AOT execution for this build')
assert log.readLines().count { it.contains('Running Micronaut AOT') } == 1

File effectiveConfig = new File(basedir, 'target/aot/jit/effective-aot.properties')
assert effectiveConfig.exists()

File jarFilePath = new File(basedir, 'target/dual-plugin-aot-0.1.jar')
assert jarFilePath.exists()

JarFile jarFile = new JarFile(jarFilePath)
assert jarFile.stream().anyMatch { entry -> entry.name == 'io/micronaut/build/examples/generated/AOTApplicationContextConfigurer.class' }
