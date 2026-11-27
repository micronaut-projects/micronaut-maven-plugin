File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
assert text.contains("BUILD SUCCESS")
assert !text.contains("BUILD FAILURE")

// both goals start the server for the application, selected from the reactor, which enables Test Resources itself
assert text.count("Starting Micronaut Test Resources service") == 2 : "Test Resources service was not started by both goals"
// and launch with its client, which the application does not declare
assert text.count("micronaut-test-resources-client:4") == 2 && text.count("to the launch classpath: Test Resources is enabled") == 2

// mn:test: the test reads the property the server resolves
File report = new File(basedir, 'app/target/surefire-reports/TEST-testtr.app.GreetingTest.xml')
assert report.exists()
def suite = new groovy.xml.XmlSlurper().parse(report)
assert suite.@tests.toInteger() == 1
assert suite.@failures.toInteger() == 0
assert suite.@errors.toInteger() == 0

// mn:dev: the application reads it
assert text.contains("greeting=Hello from the test resources server")

// the server is stopped once each goal ends: it was not started standalone
assert !new File(basedir, 'app/.micronaut/test-resources/test-resources.properties').exists()
String port = new File(basedir, 'app/target/test-resources-port.txt').text.trim()
try (ServerSocket socket = new ServerSocket(port as int)) {
    assert socket != null
}
