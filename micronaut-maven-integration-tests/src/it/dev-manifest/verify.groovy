File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Development mode manifest written to")

File manifestFile = new File(basedir, 'target/micronaut-dev/dev.properties')
assert manifestFile.exists()
Properties manifest = new Properties()
manifestFile.withInputStream { manifest.load(it) }

assert manifest.'micronaut.dev.main-class' == 'devmanifest.Application'
assert manifest.'micronaut.dev.strategy' == 'restart'
assert manifest.'micronaut.dev.compile.mode' == 'embedded'
assert manifest.'micronaut.dev.compile.incremental' == 'true'
assert manifest.'micronaut.dev.build-tool' == 'maven'
assert manifest.'micronaut.dev.build-tool.trigger'.endsWith('micronaut-dev' + File.separator + 'reload')
assert manifest.'micronaut.dev.retain' == 'javax.sql.DataSource'
assert manifest.'micronaut.dev.livereload.port' == '35730'
assert manifest.'micronaut.dev.livereload.inject-script' == 'true'
assert manifest.'micronaut.dev.project-dir' == basedir.absolutePath
assert manifest.'micronaut.dev.sources.java'.endsWith('src' + File.separator + 'main' + File.separator + 'java')
assert manifest.'micronaut.dev.resources.config'.endsWith('src' + File.separator + 'main' + File.separator + 'resources')
assert manifest.'micronaut.dev.resources.static'.endsWith('static')
assert manifest.'micronaut.dev.reloadable'.endsWith('target' + File.separator + 'classes')
assert manifest.'micronaut.dev.compile.java.output'.endsWith('target' + File.separator + 'classes')
assert manifest.'micronaut.dev.compile.java.generated-sources'.contains('generated-sources')

assert manifest.'micronaut.dev.runtime-classpath' == '@runtime.argfile'
List<String> runtime = new File(basedir, 'target/micronaut-dev/runtime.argfile').readLines()
assert runtime.any { it.contains('micronaut-runtime-') }
assert runtime.every { !it.contains('target' + File.separator + 'classes') }
assert manifest.'micronaut.dev.compile-classpath' == '@compile.argfile'
assert manifest.'micronaut.dev.processor-path' == '@processors.argfile'
List<String> processors = new File(basedir, 'target/micronaut-dev/processors.argfile').readLines()
assert processors.any { it.contains('micronaut-http-validation-') }
assert processors.any { it.contains('micronaut-inject-java-') }
// the path's exclusion is honoured, as Maven honours it
assert processors.every { !it.contains('reactor-core-') }
assert manifest.'micronaut.dev.compile.java.options' == '@java-options.argfile'
List<String> options = new File(basedir, 'target/micronaut-dev/java-options.argfile').readLines()
assert options.containsAll(['-parameters', '-Amicronaut.processing.group=devmanifest', '-Amicronaut.processing.module=dev-manifest', '-Xlint:unchecked,deprecation'])
assert options.contains('--release') || options.contains('-source')
assert manifest.'micronaut.dev.generations' == new File(basedir, 'target/micronaut-dev/generations').absolutePath
