File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

// Should validate once (first run) and hit cache on the second run
assert (text.count('Validating Micronaut configuration (package)') == 1)
assert (text.count('Validating Micronaut configuration (test)') == 1)

def packageReport = new File(basedir, 'target/micronaut/config-validation/package/configuration-errors.json')
def testReport = new File(basedir, 'target/micronaut/config-validation/test/configuration-errors.json')
assert packageReport.exists()
assert testReport.exists()

def packageCache = new File(basedir, 'target/micronaut/config-validation/package/.cache.properties')
def testCache = new File(basedir, 'target/micronaut/config-validation/test/.cache.properties')
assert packageCache.exists()
assert testCache.exists()
