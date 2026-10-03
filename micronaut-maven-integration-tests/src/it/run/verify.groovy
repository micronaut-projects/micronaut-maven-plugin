File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
assert text.count("Hi!") == 3
assert !text.contains("Micronaut AOT")

// One value per invocation, in the order of invoker.properties: the default
// command line leaves the JMX agent off, and both opt-ins turn it back on
assert text.findAll(/jmxremote property set: (true|false)/) { match, value -> value } == ['false', 'true', 'true']
assert text.findAll(/JMX agent started: (true|false)/) { match, value -> value } == ['false', 'true', 'true']
