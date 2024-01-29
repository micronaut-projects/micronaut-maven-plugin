File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("fnproject/fn-java-fdk:jre17-latest")
assert log.text.contains("Successfully tagged alvarosanchez/package-docker-oracle-function:0.1")