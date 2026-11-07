import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration
import java.util.concurrent.TimeUnit

// a module of the reactor edited under mn:dev leaves nothing in the application's output, so that a package without clean,
// after a stop of any kind, jars what the sources say and nothing of the other modules

File base = basedir as File
File log = new File(base, 'build.log')
assert log.text.contains("BUILD SUCCESS")

File mvnw = new File(base, '../../../mvnw').canonicalFile
assert mvnw.exists()
String testsRepo = new File(base, '../../../target/local-repo').canonicalPath
// mn:dev resolves into a repository of its own: the plugin under test from the tests' repository, Micronaut Core
// 5.3.0-SNAPSHOT from the Central Portal snapshots repository, as the scenario's own build did, and everything else
// from Maven Central, which the super POM declares
String localRepo = new File(base, 'target/dev-reload-reactor-repo').path
File settings = new File(base, 'target/dev-reload-reactor-settings.xml')
def repository = { String id, File directory ->
    """        <repository>
          <id>$id</id>
          <url>${directory.toURI()}</url>
          <releases><enabled>true</enabled><checksumPolicy>ignore</checksumPolicy></releases>
          <snapshots><enabled>true</enabled><checksumPolicy>ignore</checksumPolicy></snapshots>
        </repository>"""
}
settings.text = """<settings>
  <profiles>
    <profile>
      <id>dev-reload-reactor</id>
      <repositories>
${repository('tests', new File(testsRepo))}
        <repository>
          <id>central-portal-snapshots</id>
          <url>https://central.sonatype.com/repository/maven-snapshots/</url>
          <releases><enabled>false</enabled></releases>
          <snapshots><enabled>true</enabled></snapshots>
        </repository>
      </repositories>
      <pluginRepositories>
${repository('tests', new File(testsRepo)).replace('repository>', 'pluginRepository>')}
      </pluginRepositories>
    </profile>
  </profiles>
  <activeProfiles><activeProfile>dev-reload-reactor</activeProfile></activeProfiles>
</settings>
"""

int port = new ServerSocket(0).withCloseable { it.localPort }
HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2)).build()

def get = { String path ->
    try {
        HttpResponse<String> response = client.send(
                HttpRequest.newBuilder(URI.create("http://localhost:$port$path")).timeout(Duration.ofSeconds(5)).build(),
                HttpResponse.BodyHandlers.ofString())
        response.statusCode() == 200 ? response.body() : null
    } catch (IOException ignored) {
        null
    }
}

File greeter = new File(base, 'lib/src/main/java/devreactor/lib/Greeter.java')
def write = { String greeting ->
    long previous = greeter.lastModified()
    greeter.text = greeter.text.replaceAll('return "[^"]*"', "return \"$greeting\"")
    // a watcher comparing modification times sees the edit even within the file system's resolution
    if (greeter.lastModified() <= previous) {
        greeter.setLastModified(previous + 2000)
    }
}

def maven = { File output, String... goals ->
    List<String> command = [mvnw.path, '-ntp', '-B', "-Dmaven.repo.local=$localRepo".toString(), '-s', settings.path]
    command.addAll(goals)
    new ProcessBuilder(command).directory(base).redirectErrorStream(true).redirectOutput(output).start()
}

def session = { File output ->
    Process mvn = maven(output, 'mn:dev', '-Dmn.dev.livereload=false', "-Dmn.jvmArgs=-Dmicronaut.server.port=$port".toString())
    def await = { String expected, Duration timeout ->
        long deadline = System.nanoTime() + timeout.toNanos()
        String last = null
        while (System.nanoTime() < deadline) {
            assert mvn.alive : "mn:dev ended before /hello answered:\n" + output.text
            last = get('/hello')
            if (last == expected) {
                return
            }
            Thread.sleep(250)
        }
        assert false : "/hello did not answer $expected within $timeout: last answer '$last'\n" + output.text
    }
    [mvn, await]
}

File siblings = new File(base, 'app/target/micronaut-dev/siblings')
List<String> applicationOutput = {
    File classes = new File(base, 'app/target/classes')
    List<String> files = []
    classes.eachFileRecurse { if (it.file) { files << classes.toPath().relativize(it.toPath()).toString() } }
    files
}

def packaged = { String expected ->
    File log = new File(base, "package-${expected.replace(' ', '-')}.log")
    Process mvn = maven(log, 'package', '-DskipTests')
    assert mvn.waitFor(5, TimeUnit.MINUTES) && mvn.exitValue() == 0 : log.text
    new java.util.zip.ZipFile(new File(base, 'app/target/original-app-0.1.jar')).withCloseable { jar ->
        List<String> names = jar.entries().toList()*.name
        assert names.contains('devreactor/app/HelloController.class')
        assert names.every { !it.contains('devreactor/lib') && !it.contains('devreactor.lib') } : names
    }
    Process app = new ProcessBuilder(System.getProperty('java.home') + '/bin/java', "-Dmicronaut.server.port=$port".toString(), '-jar', new File(base, 'app/target/app-0.1.jar').path)
            .directory(base).redirectErrorStream(true).redirectOutput(new File(base, 'jar.log')).start()
    try {
        String answer = null
        long deadline = System.nanoTime() + Duration.ofMinutes(1).toNanos()
        while (answer == null && System.nanoTime() < deadline) {
            answer = get('/hello')
            Thread.sleep(250)
        }
        assert answer == expected
    } finally {
        app.destroyForcibly()
        app.waitFor(30, TimeUnit.SECONDS)
    }
}

Duration reload = Duration.ofSeconds(60)
def stopAll = { Process mvn ->
    List<ProcessHandle> processes = [mvn.toHandle()] + mvn.toHandle().descendants().toList()
    processes.reverse().each { it.destroyForcibly() }
    processes.each { it.onExit().get(30, TimeUnit.SECONDS) }
}

// stopped as Ctrl+C would, then killed: neither leaves anything for the package, nor for the next session to read
for (boolean kill : [false, true]) {
    String before = kill ? 'three' : 'one'
    String edited = kill ? 'four' : 'two'
    def (Process mvn, Closure await) = session(new File(base, "dev-${kill ? 'killed' : 'stopped'}.log"))
    try {
        await(before, Duration.ofMinutes(3))
        assert new File(siblings, 'classes').list().length == 0 : "what the last session compiled was kept"
        write(edited)
        await(edited, reload)
        assert new File(siblings, 'classes/devreactor/lib/Greeter.class').file
        assert applicationOutput().every { !it.contains('devreactor/lib') && !it.contains('devreactor.lib') } : applicationOutput()
    } finally {
        if (kill) {
            stopAll(mvn)
        } else {
            mvn.toHandle().destroy()
            mvn.waitFor(60, TimeUnit.SECONDS)
            stopAll(mvn)
        }
    }
    String after = kill ? 'after the kill' : 'three'
    write(after)
    packaged(after)
}
