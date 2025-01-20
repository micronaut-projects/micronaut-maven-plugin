File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")

def fhirFile = new File(basedir, "target/generated/jsonschema/src/main/java/io/micronaut/jsonschema/generated/Fhir.java")
assert fhirFile.exists()
