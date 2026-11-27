import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration
import java.util.concurrent.TimeUnit

// mn:dev end to end: the application is launched, a controller is edited and another added while it runs, and the
// new code answers from the same process; stopping Maven stops the application and leaves no process behind

File base = basedir as File
File log = new File(base, 'build.log')
assert log.text.contains("BUILD SUCCESS")

File mvnw = new File(base, '../../../mvnw').canonicalFile
assert mvnw.exists()
String testsRepo = new File(base, '../../../target/local-repo').canonicalPath
// mn:dev resolves into a repository of its own: the plugin under test from the tests' repository, Micronaut Core
// 5.3.0-SNAPSHOT from the Central Portal snapshots repository, as the scenario's own build did, and everything else
// from Maven Central, which the super POM declares
String localRepo = new File(base, 'target/dev-reload-repo').path
File settings = new File(base, 'target/dev-reload-settings.xml')
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
      <id>dev-reload</id>
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
  <activeProfiles><activeProfile>dev-reload</activeProfile></activeProfiles>
</settings>
"""

int port = new ServerSocket(0).withCloseable { it.localPort }
File devLog = new File(base, 'dev.log')
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

Process mvn = new ProcessBuilder(mvnw.path, '-ntp', '-B', "-Dmaven.repo.local=$localRepo", '-s', settings.path,
        'mn:dev', '-Dmn.dev.livereload=false', "-Dmn.jvmArgs=-Dmicronaut.server.port=$port")
        .directory(base)
        .redirectErrorStream(true)
        .redirectOutput(devLog)
        .start()

def await = { String path, Closure<Boolean> accept, Duration timeout ->
    long deadline = System.nanoTime() + timeout.toNanos()
    String last = null
    while (System.nanoTime() < deadline) {
        assert mvn.alive : "mn:dev ended before $path answered:\n" + devLog.text
        last = get(path)
        if (accept(last)) {
            return last
        }
        Thread.sleep(250)
    }
    assert false : "$path did not answer as expected within $timeout: last answer '$last'\n" + devLog.text
}

def write = { String name, String path, String body ->
    File source = new File(base, "src/main/java/devreload/${name}.java")
    long previous = source.exists() ? source.lastModified() : 0
    source.text = """package devreload;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Produces;

@Controller("$path")
public class $name {
    @Get
    @Produces(MediaType.TEXT_PLAIN)
    public String index() {
        return "$body";
    }
}
"""
    // a watcher comparing modification times sees the edit even within the file system's resolution
    if (source.lastModified() <= previous) {
        source.setLastModified(previous + 2000)
    }
}

def seconds = { long since -> String.format('%.1f', (System.nanoTime() - since) / 1e9d) }

Duration reload = Duration.ofSeconds(60)
ProcessHandle maven = mvn.toHandle()
List<ProcessHandle> processes = []
try {
    long started = System.nanoTime()
    await('/hello', { it == 'one' }, Duration.ofMinutes(3))
    long pid = Long.parseLong(await('/pid', { it != null }, reload))
    ProcessHandle launcher = ProcessHandle.of(pid).orElseThrow()
    println "mn:dev answered after ${seconds(started)}s"

    long edited = System.nanoTime()
    write('HelloController', '/hello', 'two')
    await('/hello', { it == 'two' }, reload)
    assert await('/pid', { it != null }, reload) == String.valueOf(pid) : "the edit restarted the process"
    println "the edit answered after ${seconds(edited)}s"

    long added = System.nanoTime()
    write('AddedController', '/added', 'added')
    await('/added', { it == 'added' }, reload)
    assert await('/pid', { it != null }, reload) == String.valueOf(pid) : "the new controller restarted the process"
    assert get('/hello') == 'two'
    println "the new controller answered after ${seconds(added)}s"

    // Maven is stopped as Ctrl+C would: the JVM running the goal, which launched the launcher, is signalled
    ProcessHandle goal = launcher.parent().orElseThrow()
    processes = [maven] + maven.descendants().toList()
    assert processes.contains(goal) && processes.contains(launcher)
    goal.destroy()
    assert mvn.waitFor(60, TimeUnit.SECONDS) : "Maven did not stop"
    launcher.onExit().get(30, TimeUnit.SECONDS)
    List<ProcessHandle> leftover = processes.findAll { it.alive }
    assert leftover.isEmpty() : "processes left running: " + leftover.collect { it.pid() + " " + it.info().command().orElse('') }
    String text = devLog.text
    assert !text.contains("BUILD FAILURE") && !text.contains("Exception in thread")
    try (ServerSocket socket = new ServerSocket(port)) {
        assert socket != null
    }
    println "mn:dev ran for ${seconds(started)}s"
} finally {
    // a failed run leaves nothing behind
    (processes ?: [maven] + maven.descendants().toList()).reverse().each { it.destroyForcibly() }
}
