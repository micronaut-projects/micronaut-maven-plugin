File reportDir = new File(basedir, 'target/micronaut/config-validation/package')
File reportJson = new File(reportDir, 'configuration-errors.json')
File cacheFile = new File(reportDir, '.cache.properties')
assert reportJson.exists()
assert cacheFile.exists()
assert cacheFile.text.contains('lastResult=FAILURE')

// Run a second time without cleaning. It should fail again due to cached failure.
File mvnw = new File(basedir, '../../../mvnw')
assert mvnw.exists()

def pb = new ProcessBuilder(mvnw.absolutePath, '-ntp', '-q', 'mn:validate-configuration')
        .directory(basedir as File)
        .redirectErrorStream(true)

File outFile = new File(basedir, 'second-run.log')
pb.redirectOutput(outFile)

Process p = pb.start()
p.waitFor()
assert p.exitValue() != 0

def out = outFile.text
assert out.contains('Micronaut configuration is not valid (cached). Report:')
assert out.contains('configuration-errors.')
