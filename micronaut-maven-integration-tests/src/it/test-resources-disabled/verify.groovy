File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert !log.text.contains("test-resources-client")
assert !log.text.contains("Test Resources is configured in shared mode")

assert !new File(basedir, ".micronaut/test-resources/test-resources.properties").exists()