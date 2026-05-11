File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('Native executable not found:')
assert log.text.contains('target/missing-native-image')
