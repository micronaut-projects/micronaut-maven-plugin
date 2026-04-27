File mvnw = new File(basedir, '../../../mvnw')
assert mvnw.exists()

String localRepo = (basedir as File).toPath().resolve("../../../target/local-repo").toFile().absolutePath

def processBuilder = new ProcessBuilder(mvnw.absolutePath, "-ntp", "-q", "-Dmaven.repo.local=${localRepo}", "-Psetup",  "mn:start-testresources-service")
        .directory(basedir as File)
        .inheritIO()

Process p = processBuilder.start()
p.waitFor()

assert p.exitValue() == 0

String port = new File(basedir, "target/test-resources-port.txt").text.trim()
int retries = 10
Optional<ProcessHandle> testResourcesProcess = Optional.empty()
while (retries-- > 0 && testResourcesProcess.isEmpty()) {
    def pidCommand = new ProcessBuilder("lsof", "-ti", "tcp:${port}")
            .directory(basedir as File)
            .redirectErrorStream(true)
            .start()
    String pid = pidCommand.inputStream.text.trim()
    pidCommand.waitFor()
    if (!pid.isBlank()) {
        testResourcesProcess = ProcessHandle.of(Long.parseLong(pid.readLines().first()))
    } else {
        Thread.sleep(100L * (10 - retries))
    }
}

if (testResourcesProcess.isPresent()) {
    testResourcesProcess.get().destroyForcibly();
} else {
    assert false : "Test Resources Service process not found for port $port"
}
