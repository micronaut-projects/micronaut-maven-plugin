File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Pushing image: alvarosanchez/demo:0.1")