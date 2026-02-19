File report = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert report.exists()
assert report.text.contains('[]')
