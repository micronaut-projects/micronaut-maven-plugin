File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Built image to Docker daemon as alvarosanchez/demo:0.1")