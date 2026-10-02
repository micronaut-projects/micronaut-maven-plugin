File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text
List<String> lines = text.readLines()

assert text.contains('Launch 1 started')
assert text.contains('Launch 2 started')
assert text.contains('Launch 3 stops Maven')
assert !text.contains('Timed out waiting for the CDS archive')

// The launch commands, from RunMojo's debug log
List<String> commands = lines.findAll { it.startsWith('[DEBUG] Running ') && it.contains(' -classpath ') }
assert commands.size() == 3 : commands
// 1. records its class list
assert commands[0].contains('-XX:DumpLoadedClassList=')
assert !commands[0].contains('-XX:SharedArchiveFile=')
// 2. runs while the archive is created (or right after a dump that finished first), and records nothing
assert !commands[1].contains('-XX:DumpLoadedClassList=')
// 3. uses the archive, without a probe
assert commands[2].contains('-XX:SharedArchiveFile=')
assert commands[2].contains('-Xlog:cds*=off,aot*=off')
assert lines.any { it.startsWith('[DEBUG] Class data sharing: launching with ') && it.contains('without a probe') }

// The plugin stopped the first launch on the restart, kept its class list and created the archive in the background
assert lines.count { it.startsWith('[INFO] Class data sharing: created ') } == 1
File directory = new File(basedir, 'target/mn-cds')
File[] archives = directory.listFiles({ File file -> file.name.endsWith('.jsa') } as FileFilter)
assert archives.length == 1
String key = archives[0].name - '.jsa'
// one probe, after the dump, and none on the restart that uses the archive
String dumpLog = new File(directory, key + '.log').text
assert dumpLog.readLines().count { it.contains(' version "') } == 1

// The third launch loads the dependency classes from the archive; the first one does not
List<String> pids = new File(basedir, 'target/launches.txt').readLines()
assert pids.size() == 3
String first = new File(basedir, "target/class-load-${pids[0]}.log").text
String third = new File(basedir, "target/class-load-${pids[2]}.log").text
assert !first.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')
assert third.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')
assert third.readLines().any { it.contains('io.micronaut.build.examples.Application source: file:') && it.contains('target/classes') }

assert !lines.any { it.contains('[cds') || it.contains('[aot') }
