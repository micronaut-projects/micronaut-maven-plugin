File reportDir = new File(basedir, 'target/micronaut/config-validation/package')
File reportJson = new File(reportDir, 'configuration-errors.json')
File cacheFile = new File(reportDir, '.cache.properties')
assert reportJson.exists()
assert cacheFile.exists()
assert cacheFile.text.contains('lastResult=SUCCESS')

File mvnw = new File(basedir, '../../../mvnw')
File localRepo = new File(basedir, '../../../target/local-repo')
assert mvnw.exists()
assert localRepo.exists()

def pb = new ProcessBuilder(
        mvnw.absolutePath,
        '-ntp',
        '-q',
        "-Dmaven.repo.local=${localRepo.absolutePath}",
        'mn:validate-configuration'
)
        .directory(basedir as File)
        .redirectErrorStream(true)

File outFile = new File(basedir, 'second-run.log')
pb.redirectOutput(outFile)

Process p = pb.start()
p.waitFor()
assert p.exitValue() == 0

def out = outFile.text
assert out.contains('No configuration validation errors.')
