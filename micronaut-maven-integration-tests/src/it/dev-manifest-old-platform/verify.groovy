File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Development mode manifest written to")

// Control Panel requires Micronaut Platform 5.2.4 or later: on 5.2.3 it is not added, and one line says so
String skipped = "Micronaut Control Panel requires Micronaut Platform 5.2.4 or later; not adding it (the build uses 5.2.3)"
assert log.text.count(skipped) == 1
assert !log.text.contains("Adding Micronaut Control Panel to the development runtime")

List<String> runtime = new File(basedir, 'target/micronaut-dev/runtime.argfile').readLines()
assert runtime.any { it.contains('micronaut-runtime-') }
assert runtime.every { !it.contains('micronaut-control-panel-') }
assert !new File(basedir, 'target/micronaut-dev/control-panel').exists()
