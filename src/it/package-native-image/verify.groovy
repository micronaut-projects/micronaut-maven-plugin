File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("native-image-maven-plugin:${nativeImagePluginVersion}:native-image")
assert log.text.contains("micronaut-maven-plugin:${pluginVersion}:graalvm-resources")
//assert log.text.contains("--no-fallback")
//assert log.text.contains("-H:Class=io.micronaut.build.examples.Application")
//assert log.text.contains("-H:Name=package-native-image")
File resourceConfigFile = new File(basedir, 'target/classes/META-INF/native-image/io.micronaut.build.examples/package-native-image/resource-config.json')
def resourceConfigJson = new groovy.json.JsonSlurper().parse(resourceConfigFile)

assert resourceConfigJson.resources.pattern.any { it == "\\Qapplication.yml\\E" }
assert resourceConfigJson.resources.pattern.any { it == "\\QMETA-INF/swagger/app-0.0.yml\\E" }
assert resourceConfigJson.resources.pattern.any { it == "\\QMETA-INF/swagger/views/swagger-ui/index.html\\E" }

File fatJar = new File(basedir, "target/package-native-image-0.1.jar")
assert fatJar.exists()