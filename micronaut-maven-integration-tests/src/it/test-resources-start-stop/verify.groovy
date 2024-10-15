File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")

String port = new File(basedir, "target/test-resources-port.txt").text
int retries = 10
while (retries-- > 0) {
    try (ServerSocket s = new ServerSocket(port as int)) {
        assert s != null
        break
    } catch (IOException e) {
        Thread.sleep(100 * (10 - retries))
    }
}

if (retries == 0) {
    assert false : "Failed to connect to Test Resources Service port $port"
}