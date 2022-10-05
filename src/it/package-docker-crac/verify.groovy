File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Successfully tagged package-docker-crac-crac-checkpoint:latest")
assert log.text.contains("Successfully tagged alvarosanchez/demo:0.1")