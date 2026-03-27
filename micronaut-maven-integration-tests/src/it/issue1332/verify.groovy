import java.net.ServerSocket
import java.util.Properties

File log = new File(basedir, "build.log")
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert !log.text.contains("expected: <2> but was: <3>")
assert !log.text.contains("relation \"user\" already exists")
assert !log.text.contains("duplicate key value violates unique constraint")

File app1Report = new File(basedir, "modules/app1/target/surefire-reports/TEST-io.micronaut.build.examples.DemoTest.xml")
File app2Report = new File(basedir, "modules/app2/target/surefire-reports/TEST-io.micronaut.build.examples.DemoTest.xml")
assert app1Report.exists()
assert app2Report.exists()
assert app1Report.text.contains("failures=\"0\"")
assert app2Report.text.contains("failures=\"0\"")

Properties app1Properties = new Properties()
Properties app2Properties = new Properties()
File app1ScopeFile = new File(basedir, "modules/app1/target/test-classes/application-test.properties")
File app2ScopeFile = new File(basedir, "modules/app2/target/test-classes/application-test.properties")
assert app1ScopeFile.exists()
assert app2ScopeFile.exists()
app1ScopeFile.withInputStream { app1Properties.load(it) }
app2ScopeFile.withInputStream { app2Properties.load(it) }
assert app1Properties.getProperty("micronaut.test.resources.scope")
assert app2Properties.getProperty("micronaut.test.resources.scope")
assert app1Properties.getProperty("micronaut.test.resources.scope") != app2Properties.getProperty("micronaut.test.resources.scope")

def matcher = (log.text =~ /A Micronaut Test Resources server is listening on port (\d+)/)
assert matcher.find()
String lastPort = matcher.group(1)
while (matcher.find()) {
    lastPort = matcher.group(1)
}

try (ServerSocket socket = new ServerSocket(lastPort as int)) {
    assert socket != null
} catch (IOException e) {
    assert false : "Shared test-resources port was not released"
}
