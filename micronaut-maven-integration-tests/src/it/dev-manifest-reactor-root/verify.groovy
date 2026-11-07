File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Running project app")

Properties manifest = new Properties()
new File(basedir, 'app/target/micronaut-dev/dev.properties').withInputStream { manifest.load(it) }

// invoked from an aggregator the application does not inherit from: what resolved only against it does not remain
assert manifest.'micronaut.dev.main-class' == 'devroot.Application'
assert manifest.'micronaut.dev.retain' == null
assert manifest.'micronaut.dev.strategy' == 'auto'
