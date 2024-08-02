File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD FAILURE")
assert log.text.contains("Starting Micronaut Test Resources service")
assert !log.text.contains("Test Resources is configured in shared mode")

String port = new File(basedir, "target/test-resources-port.txt").text
try (ServerSocket s = new ServerSocket(port as int)) {
    assert s != null
} catch (IOException e) {
    assert false
}
