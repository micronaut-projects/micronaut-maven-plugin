File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Running project app")

File manifestFile = new File(basedir, 'app/target/micronaut-dev/dev.properties')
assert manifestFile.exists()
Properties manifest = new Properties()
manifestFile.withInputStream { manifest.load(it) }

String sep = File.pathSeparator
List<String> reloadable = manifest.'micronaut.dev.reloadable'.split(sep).toList()
assert reloadable[0].endsWith('app' + File.separator + 'target' + File.separator + 'classes')
assert reloadable.any { it.endsWith('lib' + File.separator + 'target' + File.separator + 'classes') }
assert reloadable.any { it.endsWith('common' + File.separator + 'target' + File.separator + 'classes') }

List<String> sources = manifest.'micronaut.dev.sources.java'.split(sep).toList()
assert sources.any { it.endsWith('app' + File.separator + 'src' + File.separator + 'main' + File.separator + 'java') }
assert sources.any { it.endsWith('lib' + File.separator + 'src' + File.separator + 'main' + File.separator + 'java') }
assert sources.any { it.endsWith('common' + File.separator + 'src' + File.separator + 'main' + File.separator + 'java') }
assert sources.size() == 3
assert sources.every { !it.contains('generated-sources') }
assert manifest.'micronaut.dev.compile.java.output'.endsWith('app' + File.separator + 'target' + File.separator + 'classes')

List<String> runtime = new File(basedir, 'app/target/micronaut-dev/runtime.argfile').readLines()
assert runtime.every { !it.contains('target' + File.separator + 'classes') && !it.contains('devmulti') }
List<String> options = new File(basedir, 'app/target/micronaut-dev/java-options.argfile').readLines()
assert options.contains('-Amicronaut.processing.group=devmulti')

// invoked from the reactor root, the goal reads the selected application's own plugin configuration
assert manifest.'micronaut.dev.main-class' == 'devmulti.app.Application'
assert manifest.'micronaut.dev.strategy' == 'restart'
assert manifest.'micronaut.dev.retain' == 'devmulti.lib.Greeter'
assert manifest.'micronaut.dev.livereload.port' == '35731'
