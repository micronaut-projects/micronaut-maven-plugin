File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Hi!")
assert !log.text.contains("Micronaut AOT")

File agentOutputDirectory = new File(basedir, "target/native/agent-output/main")
assert agentOutputDirectory.exists()

int currentJdkVersion = Integer.parseInt(System.getProperty("java.specification.version"))
if (currentJdkVersion >= 23) {
    assert new File(agentOutputDirectory, "reachability-metadata.json").exists()
} else {
    assert new File(agentOutputDirectory, "reflect-config.json").exists()
}
