File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Hi!")
assert !log.text.contains("Micronaut AOT")

File agentOutputDirectory = new File(basedir, "target/native/agent-output/main")
assert agentOutputDirectory.exists()

String javaSpecVersion = System.getProperty("java.specification.version")
int currentJdkVersion
if (javaSpecVersion.startsWith("1.")) {
    currentJdkVersion = Integer.parseInt(javaSpecVersion.substring(2))
} else {
    int dotIndex = javaSpecVersion.indexOf('.')
    String majorVersion = dotIndex == -1 ? javaSpecVersion : javaSpecVersion.substring(0, dotIndex)
    currentJdkVersion = Integer.parseInt(majorVersion)
}

if (currentJdkVersion >= 23) {
    assert new File(agentOutputDirectory, "reachability-metadata.json").exists()
} else {
    assert new File(agentOutputDirectory, "reflect-config.json").exists()
}
