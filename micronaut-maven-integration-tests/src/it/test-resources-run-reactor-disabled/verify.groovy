File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
assert text.contains("BUILD SUCCESS") : "Build did not succeed"

// mn:run selects the application from the reactor, whose own properties disable Test Resources
assert text.contains("test resources server uri=null") : "The application did not run, or ran with a Test Resources service"
assert !text.contains("Starting Micronaut Test Resources service") : "Test Resources service was started"
assert !new File(basedir, 'target/test-resources-port.txt').exists()
assert !new File(basedir, 'app/target/test-resources-port.txt').exists()
