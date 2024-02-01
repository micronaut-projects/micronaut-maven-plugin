File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("native:${nativeMavenPluginVersion}:compile-no-fork")
assert log.text.contains("native:${nativeMavenPluginVersion}:generateResourceConfig")

File resourceConfigFile = new File(basedir, 'target/native/generated/generateResourceConfig/resource-config.json')
def resourceConfigJson = new groovy.json.JsonSlurper().parse(resourceConfigFile)

assert resourceConfigJson.resources.includes.any { it.pattern == "\\Qapplication.yml\\E" }
assert resourceConfigJson.resources.includes.any { it.pattern == "\\QMETA-INF/swagger/app-0.0.yml\\E" }
assert resourceConfigJson.resources.includes.any { it.pattern == "\\QMETA-INF/swagger/views/swagger-ui/index.html\\E" }

File fatJar = new File(basedir, "target/package-native-image-0.1.jar")
assert fatJar.exists()