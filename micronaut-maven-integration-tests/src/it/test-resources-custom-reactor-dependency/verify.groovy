File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Build did not succeed"
assert log.text.contains("ReactorMessageTestResourcesResolver") : "Custom reactor resolver was not loaded"

File portFile = new File(basedir, "app/target/test-resources-port.txt")
assert portFile.exists() : "Test Resources port file was not created for the app module"
