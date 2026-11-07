import java.util.zip.ZipEntry
import java.util.zip.ZipFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Startup completed")

def jars = []
new File(basedir, 'target/dependency').eachFileRecurse { if (it.name.endsWith('.jar')) jars << it }
assert jars.any { it.name.startsWith('micronaut-core-') }
for (File jar : jars) {
    new ZipFile(jar).withCloseable { zip ->
        boolean signed = zip.stream().anyMatch { it.name ==~ /(?i)META-INF\/[^\/]+\.(SF|RSA|DSA|EC)/ }
        assert signed || zip.stream().allMatch { it.method == ZipEntry.STORED }: "${jar.name} has compressed entries"
    }
}
