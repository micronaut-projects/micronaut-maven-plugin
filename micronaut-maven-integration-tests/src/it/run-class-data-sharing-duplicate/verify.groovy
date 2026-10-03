File log = new File(basedir, 'build.log')
assert log.exists()
String text = log.text

assert text.readLines().count { it == '[INFO] BUILD SUCCESS' } == 2
assert text.count('Class path: ') == 1

// The duplicate is reported with the path and the JAR
String warning = text.readLines().find { it.startsWith('[WARNING] Class data sharing is off for this launch: logback.xml (in ') }
assert warning != null : text
assert warning.contains('run-class-data-sharing-duplicate-dependency-0.1.jar')

// The project's own logback.xml wins, as it does without the option
assert text.contains('PROJECT-LOGBACK Hello from the application')
assert !text.contains('DEPENDENCY-LOGBACK')

// The class path keeps the usual order: the project's target/classes first
String classpath = text.readLines().find { it.startsWith('Class path: ') } - 'Class path: '
String first = classpath.split(File.pathSeparator)[0].replace('\\', '/')
assert first.endsWith('run-class-data-sharing-duplicate/app/target/classes') : classpath

// Nothing was recorded or archived
assert !text.contains('Class data sharing: this launch records')
File directory = new File(basedir, 'app/target/mn-cds')
assert !directory.exists() || directory.listFiles().every { !(it.name.endsWith('.jsa') || it.name.endsWith('.classlist') || it.name.endsWith('.recording')) }
