FROM oracle/graalvm-ce:20.3.0-java11 AS builder
RUN gu install native-image
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image --report-unsupported-elements-at-runtime -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre11-latest AS fnfdk

FROM oraclelinux:7-slim
WORKDIR /function
RUN groupadd -g 1000 fn && useradd --uid 1000 -g fn fn
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func"]
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
