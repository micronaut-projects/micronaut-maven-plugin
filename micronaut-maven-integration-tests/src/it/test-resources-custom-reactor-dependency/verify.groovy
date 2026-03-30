File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS") : "Build did not succeed"
assert log.text.contains("Starting Micronaut Test Resources service") : "Test Resources service was not started"

File surefireReport = new File(basedir, "app/target/surefire-reports/TEST-io.micronaut.build.examples.ReactorResolverTest.xml")
assert surefireReport.exists() : "Surefire report for ReactorResolverTest was not created in the app module"
assert surefireReport.text.contains('failures="0"') : "ReactorResolverTest did not pass successfully"
assert surefireReport.text.contains('name="resolvesPropertyFromSiblingModule"') : "ReactorResolverTest did not execute the expected test method"

File portFile = new File(basedir, "app/target/test-resources-port.txt")
assert portFile.exists() : "Test Resources port file was not created for the app module"
