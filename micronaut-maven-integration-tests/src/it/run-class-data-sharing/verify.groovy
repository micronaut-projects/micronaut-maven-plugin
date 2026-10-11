File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

assert text.readLines().count { it == '[INFO] BUILD SUCCESS' } == 3
assert text.count('Application started: io.micronaut.context.DefaultApplicationContext') == 3

// 1. The first invocation records the class list and leaves the archive behind
assert text.count('Class data sharing: this launch records the classes it loads') == 1
assert text.count('Class data sharing: waiting for the CDS archive') == 1
assert text.count('Class data sharing: created ') == 1
File directory = new File(basedir, 'target/mn-cds')
File[] archives = directory.listFiles({ File file -> file.name.endsWith('.jsa') } as FileFilter)
assert archives != null && archives.length == 1
String key = archives[0].name - '.jsa'
assert new File(directory, key + '.classlist').isFile()
assert !new File(directory, key + '.failed').exists()

// The JVM loads no dependency class from the archive while recording
String recording = new File(basedir, 'target/class-load-1.log').text
assert recording.contains('io.micronaut.context.DefaultBeanContext source: file:')
assert !recording.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')

// 2. The second invocation loads the dependency classes from the archive and the application class from target/classes
String archived = new File(basedir, 'target/class-load-2.log').text
assert archived.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')
assert archived.readLines().any { it.contains('io.micronaut.build.examples.Application source: file:') && it.contains('target/classes') }

// 3. With JDWP, the debugger port opens and the dependency classes still come from the archive
assert text.contains('Listening for transport dt_socket at address:')
String debug = new File(basedir, 'target/class-load-debug.log').text
assert debug.contains('io.micronaut.context.DefaultBeanContext source: shared objects file')

// No CDS or AOT log line reaches the output, in any invocation
assert !text.readLines().any { it.contains('[cds') || it.contains('[aot') }
