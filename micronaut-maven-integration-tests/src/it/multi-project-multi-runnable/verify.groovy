File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Startup completed")
assert log.text.contains("Application1 running")
assert !log.text.contains("Application2 running")
assert !log.text.contains("Resource [logback.xml] occurs multiple times on the classpath")
assert log.text.contains("Embedded Application shutting down")

List<String> lines = log.text.readLines()
assert lines.count { it == '[INFO] BUILD SUCCESS' } == 2
List<List<String>> classpaths = lines.findAll { it.startsWith('Class path: ') }
    .collect { (it - 'Class path: ').split(File.pathSeparator).collect { entry -> entry.replace('\\', '/') } }
assert classpaths.size() == 2

def outputIndexes = { List<String> classpath ->
    [classpath.findIndexOf { it.endsWith('/lib/target/classes') }, classpath.findIndexOf { it.endsWith('/app1/target/classes') }]
}
// 1. Without class data sharing, the reactor outputs come first
assert outputIndexes(classpaths[0]) == [0, 1]

// 2. With it (JDK 25 or later), every JAR comes first, then the reactor outputs, and the goal leaves an archive behind
if (Runtime.version().feature() >= 25) {
    List<String> withCds = classpaths[1]
    int lastJar = withCds.findLastIndexOf { it.endsWith('.jar') }
    assert lastJar > 0
    assert outputIndexes(withCds) == [lastJar + 1, lastJar + 2] : withCds
    assert log.text.contains('Class data sharing: created ')
    File[] archives = new File(basedir, 'app1/target/mn-cds').listFiles({ File file -> file.name.endsWith('.jsa') } as FileFilter)
    assert archives != null && archives.length == 1
}
