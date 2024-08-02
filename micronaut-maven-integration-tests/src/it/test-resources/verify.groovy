File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Build did not succeed"
assert log.text.contains("Starting Micronaut Test Resources service") : "Test Resources service was not started"
assert !log.text.contains("Test Resources is configured in shared mode") : "Test Resources was configured in shared mode"

String port = new File(basedir, "target/test-resources-port.txt").text
try (ServerSocket s = new ServerSocket(port as int)) {
    assert s != null
} catch (IOException e) {
    assert false
}
