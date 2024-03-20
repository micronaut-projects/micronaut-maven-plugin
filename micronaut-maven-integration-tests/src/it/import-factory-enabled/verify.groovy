File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("generate-import-factory")
assert log.text.contains("matching dependencies")
assert log.text.contains("matching packages")

File factorySource = new File(basedir, 'target/generated-sources/importfactory/io/micronaut/build/examples/ImportFactory.java')
assert factorySource.exists()

File factoryClass = new File(basedir, 'target/classes/io/micronaut/build/examples/ImportFactory.class')
assert factoryClass.exists()