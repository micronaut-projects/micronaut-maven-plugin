File log = new File(basedir, 'build.log')
assert log.exists()
def text = log.text
assert text.contains("BUILD SUCCESS") : "Docker packaging should succeed"
assert !text.contains("Could not build image") : "Base image resolution should not fail"
