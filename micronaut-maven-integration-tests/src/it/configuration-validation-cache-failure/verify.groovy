File reportDir = new File(basedir, 'target/micronaut/config-validation/package')
File reportJson = new File(reportDir, 'configuration-errors.json')
File cacheFile = new File(reportDir, '.cache.properties')
assert reportJson.exists()
assert cacheFile.exists()
assert cacheFile.text.contains('lastResult=SUCCESS')

boolean windows = System.getProperty('os.name').toLowerCase().contains('windows')
File mvnw = new File(basedir, windows ? '../../../mvnw.cmd' : '../../../mvnw')
File localRepo = new File(basedir, '../../../target/local-repo')
assert mvnw.exists()
assert localRepo.exists()

def command = windows ? ['cmd.exe', '/c', mvnw.absolutePath] : [mvnw.absolutePath]
command = command.collect { it.toString() }
command.addAll([
        '-ntp',
        '-q',
        "-Dmaven.repo.local=${localRepo.absolutePath}",
        'mn:validate-configuration'
].collect { it.toString() })

def pb = new ProcessBuilder(command)
        .directory(basedir as File)
        .redirectErrorStream(true)

File outFile = new File(basedir, 'second-run.log')
pb.redirectOutput(outFile)

Process p = pb.start()
p.waitFor()
assert p.exitValue() == 0

def out = outFile.text
assert !out.contains('Micronaut configuration is not valid')
