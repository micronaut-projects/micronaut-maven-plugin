File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

// Ensure validate-configuration is executed by default when running mvn package
assert text.contains('validate-configuration')
assert text.contains('Validating Micronaut configuration (package)')
