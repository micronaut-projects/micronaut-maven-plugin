File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD FAILURE")
assert log.text.contains("The Micronaut Maven Plugin is declared in the following projects: [app1, app2]. Please specify the project to run with the -pl option.")
