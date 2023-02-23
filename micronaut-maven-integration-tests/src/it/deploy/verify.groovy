File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Pushing image: us-phoenix-1.ocir.io/oraclelabs/micronaut-maven-plugin:0.1")