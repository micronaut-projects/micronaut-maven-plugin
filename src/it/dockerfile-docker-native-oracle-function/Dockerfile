FROM ghcr.io/graalvm/graalvm-ce:java11-21.3.0 AS builder
RUN gu install native-image
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image --report-unsupported-elements-at-runtime -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre11-latest AS fnfdk

FROM frolvlad/alpine-glibc:alpine-3.12
WORKDIR /function
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func", "-XX:MaximumHeapSizePercent=80", "-Djava.library.path=/function"]
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
