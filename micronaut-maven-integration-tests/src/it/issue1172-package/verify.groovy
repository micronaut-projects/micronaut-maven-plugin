File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("[WARNING] Failed to login to registry") : "Credentials check should be a soft failure"
assert log.text.contains("Could not build image") : "Build should be attempted"
