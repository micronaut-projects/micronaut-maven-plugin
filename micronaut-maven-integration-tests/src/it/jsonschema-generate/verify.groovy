File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")

def directory = new File(basedir, "target/generated-sources/jsonschema/src/main/java/io/micronaut/jsonschema")
assert directory.list().length == 5

def animalFile = new File(basedir, "target/generated-sources/jsonschema/src/main/java/io/micronaut/jsonschema/Animal.java")
assert animalFile.exists()
