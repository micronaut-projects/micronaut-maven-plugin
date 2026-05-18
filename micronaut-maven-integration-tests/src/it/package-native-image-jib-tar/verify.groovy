File tarball = new File(basedir, 'target/jib-image.tar')
assert tarball.exists()

Process process = ['tar', '-tf', tarball.absolutePath].execute(null, basedir)
process.waitFor()
assert process.exitValue() == 0
String contents = process.inputStream.text
assert contents.contains('manifest.json')
assert contents.contains('config.json')
assert contents.readLines().any { it.endsWith('.tar.gz') }

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('Built native image container')
