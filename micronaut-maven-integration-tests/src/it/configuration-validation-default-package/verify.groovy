File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

assert text.contains('validate-configuration')
assert !text.contains('Validating Micronaut configuration (package)')

File report = new File(basedir, 'target/micronaut/config-validation/package/configuration-errors.json')
assert !report.exists()
