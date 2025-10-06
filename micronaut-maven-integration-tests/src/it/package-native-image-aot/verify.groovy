import java.util.jar.JarFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("native:${nativeMavenPluginVersion}:compile-no-fork")
assert log.text.contains("native:${nativeMavenPluginVersion}:generateTestResourceConfig")

File resourceConfigFile = new File(basedir, 'target/native/generated/generateResourceConfig/resource-config.json')
def resourceConfigJson = new groovy.json.JsonSlurper().parse(resourceConfigFile)

assert resourceConfigJson.resources.includes.any { it.pattern == "\\Qapplication.yml\\E" }
assert resourceConfigJson.resources.includes.any { it.pattern.contains "app-0.0.yml\\E" }
assert resourceConfigJson.resources.includes.any { it.pattern.contains "index.html\\E" }

File fatJar = new File(basedir, "target/package-native-image-aot-0.1.jar")
assert fatJar.exists()

JarFile jarFile = new JarFile(fatJar)

//Generated
assert jarFile.stream().anyMatch{ e -> e.name == 'io/micronaut/build/examples/generated/AOTApplicationContextConfigurer.class' }
assert jarFile.stream().anyMatch{ e -> e.name == 'META-INF/native-image/io.micronaut.build.examples.generated/native-image.properties' }

// Resource filtering - both YAML and properties files are converted to Java with property-source-loader.generate.enabled
assert jarFile.stream().noneMatch{ e -> e.name == 'application.yml' }
assert jarFile.stream().noneMatch{ e -> e.name == 'application.properties' }

assert log.text.contains("io.micronaut.runtime.Micronaut - Startup completed")