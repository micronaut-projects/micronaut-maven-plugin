import groovy.json.JsonSlurper

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

def packageJsonData = new JsonSlurper().parseText(packageJson.text)
assert packageJsonData instanceof Map
assert packageJsonData.isEmpty()

File testHtml = new File(basedir, 'target/validation-reports/test/configuration-errors.html')
File testJson = new File(basedir, 'target/validation-reports/test/configuration-errors.json')
assert testHtml.exists()
assert !testJson.exists()
