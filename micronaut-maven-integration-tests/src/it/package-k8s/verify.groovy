File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains('k8s: `k8s:build` goal is skipped.')
