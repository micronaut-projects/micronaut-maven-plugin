// runs when the build's repository holds micronaut-dev: see src/it/local-core.groovy
return evaluate(new File(itDirectory as String, 'local-core.groovy'))
