File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Pushing image: phx.ocir.io/oraclelabs/micronaut-maven-plugin:0.1")