File log = new File(basedir, 'build.log')
assert log.exists()

def text = log.text

// cacheEnabled=false => validation should run on both invocations
assert (text.count('Validating Micronaut configuration (package)') == 2)
assert (text.count('Validating Micronaut configuration (test)') == 2)

File packageJson = new File(basedir, 'target/validation-reports/package/configuration-errors.json')
File packageHtml = new File(basedir, 'target/validation-reports/package/configuration-errors.html')
assert packageJson.exists()
assert !packageHtml.exists()

// Suppression should downgrade the port type error to WARNING
def packageJsonText = packageJson.text
assert packageJsonText.contains('"property":"micronaut.server.port"')
assert packageJsonText.contains('"type":"WARNING"')
assert !packageJsonText.contains('"type":"ERROR"')

File testHtml = new File(basedir, 'target/validation-reports/test/configuration-errors.html')
File testJson = new File(basedir, 'target/validation-reports/test/configuration-errors.json')
assert testHtml.exists()
assert !testJson.exists()
