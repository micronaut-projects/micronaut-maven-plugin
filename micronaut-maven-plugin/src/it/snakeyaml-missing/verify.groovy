File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD FAILURE")
assert log.text.contains("YAML configuration file detected, but SnakeYAML is not on the runtime classpath. Make sure to add the following dependency:")
