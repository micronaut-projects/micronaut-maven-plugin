File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

assert (text.count('Validating Micronaut configuration (package)') == 2)

File cacheFile = new File(basedir, 'target/micronaut/config-validation/package/.cache.properties')
assert cacheFile.exists()

File report = new File(basedir, 'target/micronaut/config-validation/package/configuration-errors.json')
assert report.exists()
