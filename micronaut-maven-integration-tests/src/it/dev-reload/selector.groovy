// development mode is in micronaut-dev, Micronaut Core 5.3: until a 5.3 with it is published, the scenario runs when the
// repository of the build running the tests holds one, published from core with publishToMavenLocal, and is skipped without
File repository = new File(System.getProperty('maven.repo.local') ?: new File(System.getProperty('user.home'), '.m2/repository').path)
return new File(repository, 'io/micronaut/micronaut-dev/5.3.0-SNAPSHOT').isDirectory()
