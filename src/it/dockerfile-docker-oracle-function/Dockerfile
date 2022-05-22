FROM fnproject/fn-java-fdk:jre11-latest
WORKDIR /function
COPY classes /function/app/
COPY dependency/* /function/app/
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
ENTRYPOINT ["/usr/local/openjdk-11/bin/java", "-XX:-UsePerfData", "-XX:+UseSerialGC", "-Xshare:on", "-Djava.awt.headless=true", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
