File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Startup completed")
assert log.text.contains("Embedded Application shutting down")
