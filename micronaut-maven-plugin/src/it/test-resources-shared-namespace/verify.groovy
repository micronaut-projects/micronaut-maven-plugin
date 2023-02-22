File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Test Resources is configured in shared mode with the namespace: my-namespace")
assert log.text.contains("Starting Micronaut Test Resources service")
assert log.text.contains("Shutting down Micronaut Test Resources service")

String port = new File(basedir, "target/test-resources-port.txt").text
try (ServerSocket s = new ServerSocket(port as int)) {
    assert s != null
} catch (IOException e) {
    assert false
}
