File log = new File(basedir, 'build.log')
assert log.exists()
assert !log.text.contains("Cannot find Micronaut Test Resources service settings, server may already be shutdown.")
String text = log.text
assert text.readLines().any { it == "0.1" }
