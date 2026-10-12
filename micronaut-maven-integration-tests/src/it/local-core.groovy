// The selector of the scenarios that launch micronaut-dev, through mn:dev or mn:test, evaluated from their
// selector.groovy. micronaut-dev is in Micronaut Core 5.3, unreleased: until a 5.3 with it is published, the scenarios
// resolve it from the repository of the build running the tests, where core's publishToMavenLocal puts it, and run when
// it is there.
//
// A core 5.3.0-SNAPSHOT published to a snapshots repository, without micronaut-dev, wins over the local one, whose
// metadata has no timestamp, whatever the order of the repositories or their update policies. The settings declare one,
// and BOMs the platform imports, Netty's among them, declare more. So the scenarios set it.mirror-of-build-repository
// in their test.properties, and the settings' mirror resolves every repository but Maven Central from the build's.
//
// A build that resolves that snapshot into the build's repository replaces the local BOM with it, so the BOM is
// checked as well as the launcher. A copy of it the tests' repository kept from an earlier run would win too, as one
// installed there: the scenario removes the tests' copies of the core it resolves, which resolve again from the build's.
String version = micronautCoreDevVersion
if (!version.endsWith('-SNAPSHOT')) {
    return true
}
File repository = new File(buildRepository as String)
File launcher = new File(repository, "io/micronaut/micronaut-dev/$version/micronaut-dev-${version}.jar")
File bom = new File(repository, "io/micronaut/micronaut-core-bom/$version/micronaut-core-bom-${version}.pom")
String missing = null
if (!launcher.isFile()) {
    missing = "$launcher does not exist"
} else if (!bom.isFile() || !bom.text.contains('<artifactId>micronaut-dev</artifactId>')) {
    missing = "$bom does not manage micronaut-dev"
}
if (missing) {
    println "Skipping ${(basedir as File).name}: it needs io.micronaut:micronaut-dev:$version, published from Micronaut Core with " +
            "./gradlew publishToMavenLocal, and $missing"
    return false
}
File testsCore = new File(testsRepository as String, 'io/micronaut')
// unless the build runs with the tests' repository as its own, whose copies are the local ones
if (testsCore.isDirectory() && testsCore.canonicalFile != new File(repository, 'io/micronaut').canonicalFile) {
    testsCore.eachDir { File artifact ->
        File copy = new File(artifact, version)
        if (copy.isDirectory() && new File(repository, "io/micronaut/${artifact.name}/$version").isDirectory()) {
            assert copy.deleteDir()
        }
    }
}
return true
