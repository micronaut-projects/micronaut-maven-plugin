File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Hi!")
assert !log.text.contains("Micronaut AOT")