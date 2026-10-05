Properties buildInfo = new Properties()
File buildInfoFile = new File(basedir, 'target/classes/META-INF/build-info.properties')
assert buildInfoFile.exists()
buildInfoFile.withInputStream { buildInfo.load(it) }
assert buildInfo.getProperty('build.group') == 'io.micronaut.build.examples'
assert buildInfo.getProperty('build.artifact') == 'generate-info'
assert buildInfo.getProperty('build.name') == 'Generate Info Example'
assert buildInfo.getProperty('build.version') == '0.1'
assert buildInfo.getProperty('build.time') == '2026-01-02T03:04:05Z'
assert buildInfo.getProperty('build.java.source') == '25'
assert buildInfo.getProperty('build.java.target') == '25'
assert buildInfo.getProperty('build.micronaut.version') == '5.0.0-RC1'
assert buildInfo.getProperty('build.environment') == 'test'

String gitOutput(String... args) {
    def command = ['git'] + args.toList()
    def process = new ProcessBuilder(command)
            .directory(basedir as File)
            .redirectErrorStream(true)
            .start()
    def output = process.inputStream.text.trim()
    process.waitFor()
    assert process.exitValue() == 0: output
    return output
}

Properties gitInfo = new Properties()
File gitInfoFile = new File(basedir, 'target/classes/git.properties')
assert gitInfoFile.exists()
gitInfoFile.withInputStream { gitInfo.load(it) }
assert gitInfo.getProperty('git.commit.id') == gitOutput('rev-parse', 'HEAD')
assert gitInfo.getProperty('git.commit.id.abbrev') == gitOutput('rev-parse', '--short', 'HEAD')
assert gitInfo.getProperty('git.commit.time')
assert gitInfo.getProperty('git.branch')
assert gitInfo.getProperty('git.dirty') == 'true'
assert !gitInfo.containsKey('git.remote.origin.url')
assert !gitInfo.containsKey('git.build.user.name')
assert !gitInfo.containsKey('git.build.user.email')

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('Generated Micronaut build info')
assert log.text.contains('Generated Micronaut git info')
