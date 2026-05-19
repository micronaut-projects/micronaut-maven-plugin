File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Build did not succeed"
assert log.text.contains("Starting Micronaut Test Resources service") : "Test Resources service was not started"
assert log.text.contains("Added connection org.postgresql.jdbc.PgConnection") : "postgres connection was not created"
assert log.text.contains("Startup completed in") : "Startup was not completed"

File application = new File(basedir, "src/main/resources/application.yml")
assert application.text.contains("postgres:18.3@sha256:a9abf4275f9e99bff8e6aed712b3b7dfec9cac1341bba01c1ffdfce9ff9fc34a") : "pinned postgres image was not configured"

String port = new File(basedir, "target/test-resources-port.txt").text
try (ServerSocket s = new ServerSocket(port as int)) {
    assert s != null
} catch (IOException e) {
    assert false
}

assert !new File(basedir, ".micronaut/test-resources/test-resources.properties").exists()
