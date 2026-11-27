def log = new File(basedir, 'build.log').text
assert log.contains('mn:dev cannot be bound to a lifecycle phase, as it is here to prepare-package')
assert log.contains('mn:test cannot be bound to a lifecycle phase, as it is here to test')
assert new File(basedir, 'target/micronaut-dev/dev.properties').isFile() : 'the execution selected on the command line runs'
