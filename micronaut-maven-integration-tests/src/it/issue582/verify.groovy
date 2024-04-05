File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Build did not succeed"
assert !log.text.contains("MojoExecutionException")
