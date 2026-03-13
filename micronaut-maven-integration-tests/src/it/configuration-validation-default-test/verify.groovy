File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

assert text.contains('validate-test-configuration')
assert !text.contains('Validating Micronaut configuration (test)')

File report = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert !report.exists()
