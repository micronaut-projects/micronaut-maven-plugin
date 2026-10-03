Properties buildInfo = new Properties()
File buildInfoFile = new File(basedir, 'target/classes/META-INF/build-info.properties')
assert buildInfoFile.exists()
buildInfoFile.withInputStream { buildInfo.load(it) }
assert buildInfo.getProperty('build.artifact') == 'generate-info-git-disabled'
assert buildInfo.getProperty('build.time') == '2026-01-02T03:04:05Z'

File gitInfoFile = new File(basedir, 'target/classes/git.properties')
assert !gitInfoFile.exists()

File log = new File(basedir, 'build.log')
assert log.exists()
assert !log.text.contains('Generated Micronaut git info')
