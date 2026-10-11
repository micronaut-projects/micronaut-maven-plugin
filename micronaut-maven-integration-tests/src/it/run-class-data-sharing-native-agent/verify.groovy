File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

assert text.readLines().count { it == '[INFO] BUILD SUCCESS' } == 2
assert text.count('Application started: io.micronaut.context.DefaultApplicationContext') == 2
assert text.count('Class data sharing: created ') == 1

// The agent still writes its metadata
File agentOutputDirectory = new File(basedir, 'target/native/agent-output/main')
assert new File(agentOutputDirectory, 'reachability-metadata.json').exists()

// and the dependency classes still come from the archive
String loaded = new File(basedir, 'target/class-load-agent.log').text
assert loaded.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')

assert !text.readLines().any { it.contains('[cds') || it.contains('[aot') }
