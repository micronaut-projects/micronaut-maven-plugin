File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

assert (text.count('Validating Micronaut configuration (test)') == 1)

def testReport = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert testReport.exists()

def testCache = new File(basedir, 'target/micronaut/config-validation/test/.cache.properties')
assert testCache.exists()
