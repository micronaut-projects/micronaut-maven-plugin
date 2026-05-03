File dockerfile = new File("$basedir/target", "Dockerfile")
File expectedDockerfile = new File(basedir, "Dockerfile")
String expectedDockerfileText = expectedDockerfile.text

assert dockerfile.text == expectedDockerfileText

File argsFile = new File("$basedir/target").listFiles().find { it.name.endsWith(".args") }
assert argsFile != null
assert argsFile.text.contains("-H:+SharedArenaSupport")

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
