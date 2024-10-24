ARG BASE_JAVA_IMAGE=eclipse-temurin:21.0.5_11-jre

FROM fnproject/fn-java-fdk:jre17-latest AS fnfdk

FROM ${BASE_JAVA_IMAGE}
COPY --from=fnfdk /function /function
WORKDIR /function
COPY classes /function/app/classes
COPY dependency/* /function/app/libs/
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
ENTRYPOINT ["java", "-XX:-UsePerfData", "-XX:+UseSerialGC", "-Xshare:auto", "-Djava.awt.headless=true", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
