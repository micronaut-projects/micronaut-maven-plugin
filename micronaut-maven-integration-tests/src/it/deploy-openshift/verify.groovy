File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('oc: `oc:build` goal is skipped.')
assert log.text.contains('oc: `oc:push` goal is skipped.')
