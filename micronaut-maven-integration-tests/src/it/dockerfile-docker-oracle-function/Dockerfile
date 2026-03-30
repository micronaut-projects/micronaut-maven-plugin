ARG BASE_JAVA_IMAGE=eclipse-temurin:25-jre

FROM fnproject/fn-java-fdk:jre17-latest AS fnfdk

FROM ${BASE_JAVA_IMAGE}
COPY --from=fnfdk /function /function
WORKDIR /function
COPY classes /function/app/classes
COPY dependency/release/ /function/app/libs/release/
COPY dependency/snapshot/ /function/app/libs/snapshot/

CMD ["example.micronaut.Function::handleRequest"]
ENTRYPOINT ["java", "-XX:-UsePerfData", "-XX:+UseSerialGC", "-Xshare:auto", "-Djava.awt.headless=true", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/release/*:/function/app/libs/snapshot/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
