File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("native-image-maven-plugin:${nativeImagePluginVersion}:native-image")
//assert log.text.contains("--no-fallback")
//assert log.text.contains("-H:Class=io.micronaut.build.examples.Application")
//assert log.text.contains("-H:Name=package-native-image")

File fatJar = new File(basedir, "target/package-native-image-0.1.jar")
assert fatJar.exists()