import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Desugared ")
assert log.text.contains("Stripped local-variable tables")
assert log.text.contains("Replacing original artifact with shaded artifact")

File fatJar = new File(basedir, "target/package-jar-class-path-transform-0.1.jar")
assert fatJar.exists()
new JarFile(fatJar).withCloseable { jar ->
    assert jar.entries().toList().any { it.name.startsWith('io/micronaut/') && it.name.contains('$$Lambda$R') }
}

// The rewritten copies keep the dependencies' file names
File copies = new File(basedir, "target/class-path-transform/shade/copies")
assert copies.listFiles().collectMany { it.listFiles() as List }.any { it.name.startsWith("micronaut-core-") }
