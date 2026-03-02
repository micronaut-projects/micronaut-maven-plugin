File log = new File(basedir, 'build.log')
assert log.exists()
assert !log.text.contains('Micronaut configuration is not valid')

File report = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert report.exists()
assert !report.text.contains('MissingDependency')
