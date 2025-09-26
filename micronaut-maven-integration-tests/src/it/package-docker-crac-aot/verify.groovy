import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Running Micronaut AOT")

File classes = new File(basedir, "target/classes")

// AOT generates classes exist
File aotGenerated = new File(classes, "io/micronaut/build/examples/generated/AOTApplicationContextConfigurer.class")
assert aotGenerated.exists()

// Resource filtering - application.yml is preserved in AOT 2.9.0+
def list = []
classes.eachFileRecurse(groovy.io.FileType.FILES) { file ->
    if (file.name in ['application.properties']) {
        list << file
    }
}
assert list.empty

// Verify application.yml is preserved
def yamlList = []
classes.eachFileRecurse(groovy.io.FileType.FILES) { file ->
    if (file.name == 'application.yml') {
        yamlList << file
    }
}
assert !yamlList.empty

assert log.text.contains("Restore completed!")