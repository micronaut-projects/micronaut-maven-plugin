File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Development mode manifest written to")

File manifestFile = new File(basedir, 'target/micronaut-dev/test/dev.properties')
assert manifestFile.exists()
Properties manifest = new Properties()
manifestFile.withInputStream { manifest.load(it) }

String sep = File.separator
assert manifest.'micronaut.dev.mode' == 'test'
assert manifest.'micronaut.dev.main-class' == 'testgoal.Application'
assert manifest.'micronaut.dev.project-dir' == basedir.absolutePath
// the run-mode entries stay
assert manifest.'micronaut.dev.sources.java'.endsWith('src' + sep + 'main' + sep + 'java')
List<String> reloadable = manifest.'micronaut.dev.reloadable'.split(File.pathSeparator).toList()
// the test output first, as on the build's test classpath
assert reloadable == [new File(basedir, 'target/test-classes').absolutePath, new File(basedir, 'target/classes').absolutePath]
assert manifest.'micronaut.dev.compile.java.output'.endsWith('target' + sep + 'classes')
assert manifest.'micronaut.dev.build-tool.trigger'.endsWith('micronaut-dev' + sep + 'test' + sep + 'reload')
assert manifest.'micronaut.dev.livereload.port' == '35741'

assert manifest.'micronaut.dev.test.sources.java'.endsWith('src' + sep + 'test' + sep + 'java')
assert manifest.'micronaut.dev.test.resources.config'.endsWith('src' + sep + 'test' + sep + 'resources')
assert manifest.'micronaut.dev.test.compile.java.output'.endsWith('target' + sep + 'test-classes')
assert manifest.'micronaut.dev.test.compile.java.generated-sources'.contains('generated-test-sources')
assert manifest.'micronaut.dev.test.runner' == 'junit-platform'
assert manifest.'micronaut.dev.test.selection' == 'affected'
assert manifest.'micronaut.dev.test.initial-run' == 'true'
assert manifest.'micronaut.dev.test.once' == 'true'
assert manifest.'micronaut.dev.test.reports'.endsWith('target' + sep + 'surefire-reports')
assert manifest.'micronaut.dev.test.html-report'.endsWith('target' + sep + 'micronaut-dev' + sep + 'test-report')
// Surefire's -Dtest, mapped to the launcher's patterns
assert manifest.'micronaut.dev.test.filter' == 'GreeterTest,OtherTest.greets,OtherTest.counts'
assert manifest.'micronaut.dev.test.parameters.junit.jupiter.displayname.generator.default' == 'org.junit.jupiter.api.DisplayNameGenerator$Simple'

File directory = new File(basedir, 'target/micronaut-dev/test')
List<String> runtime = new File(directory, 'runtime.argfile').readLines()
assert runtime.any { it.contains('junit-jupiter-engine-') }
assert runtime.every { !it.endsWith('target' + sep + 'classes') && !it.endsWith('test-classes') }
assert manifest.'micronaut.dev.test.compile-classpath' == '@test-compile.argfile'
List<String> testCompile = new File(directory, 'test-compile.argfile').readLines()
assert testCompile[0].endsWith('target' + sep + 'classes')
assert testCompile.any { it.contains('junit-jupiter-api-') }
assert manifest.'micronaut.dev.test.processor-path' == '@test-processors.argfile'
List<String> processors = new File(directory, 'test-processors.argfile').readLines()
assert processors.any { it.contains('micronaut-inject-java-') }
List<String> options = new File(directory, 'test-java-options.argfile').readLines()
assert options.contains('-Amicronaut.processing.group=testgoal')

// the run's reports: JUnit XML in the Surefire shape, and the HTML report
File reports = new File(basedir, 'target/surefire-reports')
File greeterReport = new File(reports, 'TEST-testgoal.GreeterTest.xml')
assert greeterReport.exists()
def greeter = new groovy.xml.XmlSlurper().parse(greeterReport)
assert greeter.@tests.toInteger() == 1
assert greeter.@failures.toInteger() == 0
assert greeter.@errors.toInteger() == 0
File otherReport = new File(reports, 'TEST-testgoal.OtherTest.xml')
assert otherReport.exists()
def other = new groovy.xml.XmlSlurper().parse(otherReport)
assert other.@tests.toInteger() == 2
assert other.@failures.toInteger() == 0
assert !new File(reports, 'TEST-testgoal.FilteredOutTest.xml').exists()
File html = new File(basedir, 'target/micronaut-dev/test-report/index.html')
assert html.exists()
assert html.text.contains('GreeterTest')

// the launcher's generations go under target, beside the manifest: no build directory in a Maven project
assert manifest.'micronaut.dev.generations' == new File(basedir, 'target/micronaut-dev/test/generations').absolutePath
assert new File(basedir, 'target/micronaut-dev/test/generations').isDirectory()
assert !new File(basedir, 'build').exists()

// the LiveReload server's path for the report, from -Dmn.test.reportPath
assert manifest.'micronaut.dev.test.html-report-path' == '/test-report/'
