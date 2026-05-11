File tarball = new File(basedir, 'target/jib-image.tar')
assert tarball.exists()

Process process = ['tar', '-tf', tarball.absolutePath].execute(null, basedir)
process.waitFor()
assert process.exitValue() == 0
String contents = process.inputStream.text
assert contents.contains('oci-layout')
assert contents.contains('index.json')
assert contents.contains('blobs/sha256/')

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('Built native image container')
