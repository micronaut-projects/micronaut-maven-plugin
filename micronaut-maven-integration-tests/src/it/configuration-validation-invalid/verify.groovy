File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('Micronaut configuration is not valid')
assert !log.text.contains('If these dependency injection errors can be ignored, add the following to your pom.xml:')

File report = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert report.exists()
assert report.text.contains('"micronaut.server.ssl.enabled"')
assert report.text.contains('"Expected boolean"')
