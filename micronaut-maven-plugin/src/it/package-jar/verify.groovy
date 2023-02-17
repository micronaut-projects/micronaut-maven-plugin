File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Replacing original artifact with shaded artifact")

File fatJar = new File(basedir, "target/package-jar-0.1.jar")
assert fatJar.exists()