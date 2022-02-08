File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Sample configuration file written to")

File aotSample = new File(basedir, 'target/aot/jit/aot.properties')
assert !aotSample.text.empty
