File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")

String port = new File(basedir, "target/test-resources-port.txt").text
assert log.text.contains("Test resources service already started on port " + port)
assert log.text.contains("Shutting down Micronaut Test Resources service")

try (ServerSocket s = new ServerSocket(port as int)) {
    assert s != null
} catch (IOException e) {
    assert false
}
