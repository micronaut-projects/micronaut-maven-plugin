File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Built image to Docker daemon") : "Image should always be built"
assert log.text.contains("[WARNING] Failed to login to registry") : "Credentials check should be a soft failure"
assert log.text.contains(" Could not push image: denied") : "A push should always be attempted"
