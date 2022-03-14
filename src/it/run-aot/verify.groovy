File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Running Micronaut AOT")
assert log.text.contains("Hi!")
assert log.text.contains("Running with AOT optimizations")

Properties effectiveConfig = new Properties()
new File(basedir, 'target/aot/jit/effective-aot.properties').withReader {
    effectiveConfig.load(it)
}

Properties expectedConfig = new Properties()
new File(basedir, 'expected-effective-aot.properties').withReader {
    expectedConfig.load(it)
}
expectedConfig.each { k,v ->
    assert effectiveConfig.getProperty(k).equals(v)
}

assert effectiveConfig.size() == 10