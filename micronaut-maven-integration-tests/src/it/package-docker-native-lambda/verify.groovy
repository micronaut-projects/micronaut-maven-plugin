import java.util.zip.ZipFile

File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("BUILD SUCCESS")
assert log.text.contains("native:${nativeMavenPluginVersion}:generateTestResourceConfig")
assert log.text.contains("Successfully built")
assert log.text.contains("AWS Lambda Custom Runtime ZIP:")

File functionZip = new File(basedir, "target/function.zip")
assert functionZip.exists()

ZipFile zipFile = new ZipFile(functionZip)
try {
    def bootstrap = zipFile.getInputStream(zipFile.getEntry("bootstrap")).getText("UTF-8")
    assert bootstrap.contains('./func -XX:MaximumHeapSizePercent=80 -Dio.netty.allocator.numDirectArenas=0 -Dio.netty.noPreferDirect=true -Djava.library.path=$(pwd) -Dio.netty.noUnsafe=true -Dio.netty.noPreferDirect=false')
} finally {
    zipFile.close()
}
