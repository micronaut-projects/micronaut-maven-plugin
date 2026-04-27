File mvnw = new File(basedir, '../../../mvnw')
assert mvnw.exists()

String localRepo = (basedir as File).toPath().resolve("../../../target/local-repo").toFile().absolutePath

def processBuilder = new ProcessBuilder(mvnw.absolutePath, "-ntp", "-q", "-Dmaven.repo.local=${localRepo}", "-Psetup",  "mn:start-testresources-service")
        .directory(basedir as File)
        .inheritIO()

Process p = processBuilder.start()
p.waitFor()

assert p.exitValue() == 0

File portFile = new File(basedir, "target/test-resources-port.txt")
String port = ""
int retries = 10
Optional<ProcessHandle> testResourcesProcess = Optional.empty()
String lastLsofOutput = ""
int lastLsofExitCode = 0
while (retries-- > 0 && testResourcesProcess.isEmpty()) {
    if (portFile.exists()) {
        port = portFile.text.trim()
    }
    if (!port.isBlank()) {
        def pidCommand
        try {
            pidCommand = new ProcessBuilder("lsof", "-ti", "tcp:${port}")
                    .directory(basedir as File)
                    .redirectErrorStream(true)
                    .start()
        } catch (IOException e) {
            assert false : "Failed to execute 'lsof' to locate the Test Resources Service process for port $port. Cause: ${e.message}"
        }
        lastLsofOutput = pidCommand.inputStream.text.trim()
        lastLsofExitCode = pidCommand.waitFor()
        String pid = lastLsofOutput.readLines().find { it ==~ /\d+/ }
        if (pid != null) {
            testResourcesProcess = ProcessHandle.of(Long.parseLong(pid))
        }
    }
    if (testResourcesProcess.isEmpty()) {
        Thread.sleep(100L * (10 - retries))
    }
}

if (testResourcesProcess.isPresent()) {
    testResourcesProcess.get().destroyForcibly();
} else {
    assert false : "Test Resources Service process not found for port $port. lsof exit code: $lastLsofExitCode. Output: ${lastLsofOutput ?: '<empty>'}"
}
