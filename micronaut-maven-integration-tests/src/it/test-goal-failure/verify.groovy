File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD FAILURE")
// the launcher exits with the last run's status, and the goal fails the build on it
assert log.text.contains("The tests did not pass (status 1)")

File report = new File(basedir, 'target/surefire-reports/TEST-testfailure.CalculatorTest.xml')
assert report.exists()
def suite = new groovy.xml.XmlSlurper().parse(report)
assert suite.@tests.toInteger() == 2
assert suite.@failures.toInteger() == 1
def failed = suite.testcase.find { it.failure.size() > 0 }
assert failed.@name.text().startsWith('addsWrongly')
assert failed.failure.@message.text().contains('two and two make four')

File html = new File(basedir, 'target/micronaut-dev/test-report/index.html')
assert html.exists()
assert html.text.contains('CalculatorTest')

Properties manifest = new Properties()
new File(basedir, 'target/micronaut-dev/test/dev.properties').withInputStream { manifest.load(it) }
// the default path the LiveReload server serves the report at
assert manifest.'micronaut.dev.test.html-report-path' == '/tests/'

assert new File(basedir, 'target/micronaut-dev/test/generations').isDirectory()
assert !new File(basedir, 'build').exists()
