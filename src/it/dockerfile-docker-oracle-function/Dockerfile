FROM fnproject/fn-java-fdk:jre17-latest
WORKDIR /function
COPY classes /function/app/
COPY dependency/* /function/app/
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
ENTRYPOINT ["/usr/java/latest/bin/java", "java", "-XX:+UnlockExperimentalVMOptions", "-XX:+UseCGroupMemoryLimitForHeap", "-XX:-UsePerfData", "-XX:MaxRAMFraction=2", "-XX:+UseSerialGC", "-Xshare:on", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
