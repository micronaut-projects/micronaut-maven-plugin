File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

// Ensure validate-test-configuration is executed by default when running mvn test
assert text.contains('validate-test-configuration')
assert text.contains('Validating Micronaut configuration (test)')
