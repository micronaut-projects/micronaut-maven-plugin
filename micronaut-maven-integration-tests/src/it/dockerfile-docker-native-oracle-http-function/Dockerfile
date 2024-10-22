FROM ghcr.io/graalvm/native-image-community:23-ol9 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
COPY graalvm-reachability-metadat[a] /home/app/graalvm-reachability-metadata
COPY nativ[e]/generated /home/app/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args --report-unsupported-elements-at-runtime -H:PageSize=64k -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application -cp "/home/app/libs/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre17-latest AS fnfdk

FROM cgr.dev/chainguard/wolfi-base:latest
WORKDIR /function
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func", "-XX:MaximumHeapSizePercent=80", "-Dio.netty.allocator.numDirectArenas=0", "-Dio.netty.noPreferDirect=true", "-Djava.library.path=/function"]
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
