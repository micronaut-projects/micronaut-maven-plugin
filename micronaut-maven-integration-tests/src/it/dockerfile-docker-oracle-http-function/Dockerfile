ARG BASE_JAVA_IMAGE=eclipse-temurin:25-jre

FROM fnproject/fn-java-fdk:jre17-1.1.5@sha256:d4d50652bf27c3f66fa1f146a09d872ef686ed39307e121b621e7741d05ff593 AS fnfdk

FROM ${BASE_JAVA_IMAGE}
COPY --from=fnfdk /function /function
WORKDIR /function
COPY classes /function/app/classes
COPY dependency/* /function/app/libs/

CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
ENTRYPOINT ["java", "-XX:-UsePerfData", "-XX:+UseSerialGC", "-Xshare:auto", "-Djava.awt.headless=true", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
