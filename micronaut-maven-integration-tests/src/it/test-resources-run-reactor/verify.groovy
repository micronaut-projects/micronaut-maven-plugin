File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
assert text.contains("BUILD SUCCESS") : "Build did not succeed"

// mn:run selects the application from the reactor, whose own properties enable Test Resources
assert text.contains("Starting Micronaut Test Resources service") : "Test Resources service was not started"
// the service has the resolver the application's plugin configuration adds, and the application reads its property
assert text.contains("greeting=Hello from the test resources service") : "The application did not read the property the service resolves"

// the service ran in the application's build directory, not in the reactor root's
String port = new File(basedir, 'app/target/test-resources-port.txt').text.trim()
assert !new File(basedir, 'target/test-resources-port.txt').exists()

// and was stopped once mn:run ended
assert !new File(basedir, 'app/.micronaut/test-resources/test-resources.properties').exists()
try (ServerSocket socket = new ServerSocket(port as int)) {
    assert socket != null
}
