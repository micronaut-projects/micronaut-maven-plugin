File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Running project app")

File manifestFile = new File(basedir, 'app/target/micronaut-dev/test/dev.properties')
assert manifestFile.exists()
Properties manifest = new Properties()
manifestFile.withInputStream { manifest.load(it) }

String sep = File.separator
assert manifest.'micronaut.dev.mode' == 'test'
List<String> reloadable = manifest.'micronaut.dev.reloadable'.split(File.pathSeparator).toList()
assert reloadable[0].endsWith('app' + sep + 'target' + sep + 'test-classes')
assert reloadable[1].endsWith('app' + sep + 'target' + sep + 'classes')
assert reloadable.any { it.endsWith('lib' + sep + 'target' + sep + 'classes') }
// the lib's test-jar loads from its test output, which is reloadable as its classes are
assert reloadable.last().endsWith('lib' + sep + 'target' + sep + 'test-classes')
List<String> testCompile = new File(basedir, 'app/target/micronaut-dev/test/test-compile.argfile').readLines()
assert testCompile.any { it.endsWith('lib' + sep + 'target' + sep + 'test-classes') }
List<String> runtime = new File(basedir, 'app/target/micronaut-dev/test/runtime.argfile').readLines()
assert runtime.every { !it.contains('testmulti') && !it.endsWith('classes') }

// a relative reports directory resolves against the application, not the reactor root the goal was invoked on
assert manifest.'micronaut.dev.test.reports' == new File(basedir, 'app/target/mn-test-reports').absolutePath
File report = new File(basedir, 'app/target/mn-test-reports/TEST-testmulti.app.GreetingTest.xml')
assert report.exists()
def suite = new groovy.xml.XmlSlurper().parse(report)
assert suite.@tests.toInteger() == 1
assert suite.@failures.toInteger() == 0
assert suite.@errors.toInteger() == 0
assert new File(basedir, 'app/target/micronaut-dev/test-report/index.html').exists()

// the lib's test fixtures are watched, and compile into the app's test output, which is read first
List<String> testSources = manifest.'micronaut.dev.test.sources.java'.split(File.pathSeparator).toList()
assert testSources.size() == 2
assert testSources[0].endsWith('app' + sep + 'src' + sep + 'test' + sep + 'java')
assert testSources[1].endsWith('lib' + sep + 'src' + sep + 'test' + sep + 'java')

assert manifest.'micronaut.dev.generations' == new File(basedir, 'app/target/micronaut-dev/test/generations').absolutePath
assert !new File(basedir, 'build').exists() && !new File(basedir, 'app/build').exists()
