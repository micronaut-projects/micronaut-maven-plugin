import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('BUILD SUCCESS')
assert log.text.contains('--- mn-aot:')
assert log.text.contains('Running Micronaut AOT')

File effectiveConfig = new File(basedir, 'target/aot/jit/effective-aot.properties')
assert effectiveConfig.exists()

File jarFilePath = new File(basedir, 'target/standalone-aot-0.1.jar')
assert jarFilePath.exists()

JarFile jarFile = new JarFile(jarFilePath)
assert jarFile.stream().anyMatch { entry -> entry.name == 'io/micronaut/build/examples/generated/AOTApplicationContextConfigurer.class' }
