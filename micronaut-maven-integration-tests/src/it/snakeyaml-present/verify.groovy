File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("io.micronaut.maven.enforcer.CheckSnakeYaml passed")
