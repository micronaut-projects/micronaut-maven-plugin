File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("Goal 'graalvm-resources' is deprecated: Please use native:generateResourceConfig and/or native:generateTestResourceConfig instead.")

