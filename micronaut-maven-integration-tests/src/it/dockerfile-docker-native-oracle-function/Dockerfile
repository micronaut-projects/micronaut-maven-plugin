FROM ghcr.io/graalvm/native-image:ol8-java17-22.3.0 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args --report-unsupported-elements-at-runtime -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application -cp "/home/app/libs/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre17-latest AS fnfdk

FROM frolvlad/alpine-glibc:alpine-3.12
WORKDIR /function
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func", "-XX:MaximumHeapSizePercent=80", "-Dio.netty.allocator.numDirectArenas=0", "-Dio.netty.noPreferDirect=true", "-Djava.library.path=/function"]
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
