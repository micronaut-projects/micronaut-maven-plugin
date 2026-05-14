File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("native:${nativeMavenPluginVersion}:compile-no-fork")
assert log.text.contains('Built native image container alvarosanchez/package-native-image-jib-docker-run:0.1')
assert new File(basedir, 'target/jib-image.tar').exists()

String imageName = 'alvarosanchez/package-native-image-jib-docker-run:0.1'
Process images = ['docker', 'images', '--format', '{{.Repository}}:{{.Tag}} {{.ID}}'].execute(null, basedir)
images.waitFor()
String imagesOutput = images.inputStream.text
assert images.exitValue() == 0 : images.errorStream.text
String imageId = imagesOutput.readLines()
    .find { it.startsWith(imageName + ' ') || it.startsWith("docker.io/${imageName} ") }
    ?.split(/\s+/)
    ?.last()
assert imageId : "Image ${imageName} was not loaded. Docker images:\n${imagesOutput}"

String containerName = 'package-native-image-jib-docker-run-' + System.currentTimeMillis()
Process process = new ProcessBuilder('docker', 'run', '--rm', '--name', containerName, imageId)
    .directory(basedir)
    .redirectErrorStream(true)
    .start()
StringBuilder output = new StringBuilder()
Thread outputReader = Thread.start {
    process.inputStream.eachLine { line ->
        output.append(line).append(System.lineSeparator())
    }
}
long deadline = System.currentTimeMillis() + 20000
while (System.currentTimeMillis() < deadline && process.isAlive() && !output.toString().contains('Startup completed')) {
    Thread.sleep(250)
}
['docker', 'rm', '-f', containerName].execute(null, basedir).waitFor()
process.waitFor()
outputReader.join(5000)
assert output.toString().contains('Startup completed')
