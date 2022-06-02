File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD FAILURE")
assert log.text.contains("The io.micronaut:micronaut-parent version (3.4.0) differs from the micronaut.version property")