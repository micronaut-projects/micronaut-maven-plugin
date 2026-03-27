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

File app1PortFile = new File(basedir, "modules/app1/target/test-resources-port.txt")
File app2PortFile = new File(basedir, "modules/app2/target/test-resources-port.txt")
List<File> portFiles = [app1PortFile, app2PortFile].findAll { it.exists() }
assert !portFiles.isEmpty()
List<Integer> ports = portFiles.collect { Integer.parseInt(it.text.trim()) }
assert ports.toSet().size() == 1
int lastPort = ports.first()

try (ServerSocket socket = new ServerSocket(lastPort)) {
    assert socket != null
} catch (IOException e) {
    assert false : "Shared test-resources port was not released"
}
