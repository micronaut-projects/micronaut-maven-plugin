ARG BASE_JAVA_IMAGE=eclipse-temurin:25-jre

FROM fnproject/fn-java-fdk:jre17-1.1.7@sha256:50a0b8138fec3dde64aaedbe66406da6c2c17fc88446726c75446cb4c4fc681f AS fnfdk

FROM ${BASE_JAVA_IMAGE}
COPY --from=fnfdk /function /function
WORKDIR /function
COPY classes /function/app/classes
COPY dependency/release/ /function/app/libs/release/
COPY dependency/snapshot/ /function/app/libs/snapshot/

CMD ["example.micronaut.Function::handleRequest"]
ENTRYPOINT ["java", "-XX:-UsePerfData", "-XX:+UseSerialGC", "-Xshare:auto", "-Djava.awt.headless=true", "-Djava.library.path=/function/runtime/lib", "-cp", "/function/app/classes:/function/app/libs/release/*:/function/app/libs/snapshot/*:/function/app/resources:/function/runtime/*", "com.fnproject.fn.runtime.EntryPoint"]
