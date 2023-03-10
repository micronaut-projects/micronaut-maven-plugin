println "***********************************"
println "| Starting test resources service |"
println "***********************************"

File mvnw = new File(basedir, '../../../mvnw')
assert mvnw.exists()

def processBuilder = new ProcessBuilder(mvnw.absolutePath, "mn:start-testresources-service")
        .directory(basedir as File)
        .inheritIO()

Process p = processBuilder.start()
p.waitFor()

assert p.exitValue() == 0