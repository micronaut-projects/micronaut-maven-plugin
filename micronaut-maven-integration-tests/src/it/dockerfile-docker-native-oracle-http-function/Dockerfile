FROM ghcr.io/graalvm/native-image-community:25-ol9 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
COPY graalvm-reachability-metadata /home/app/graalvm-reachability-metadata
COPY native/generated /home/app/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args --report-unsupported-elements-at-runtime -H:PageSize=64k -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application -cp "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre17-1.1.7@sha256:50a0b8138fec3dde64aaedbe66406da6c2c17fc88446726c75446cb4c4fc681f AS fnfdk

FROM cgr.dev/chainguard/wolfi-base@sha256:a5a619c1793039dcf92f02178f37c94bb3d6001403716da59d6092dfe8d9b502
WORKDIR /function
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func", "-XX:MaximumHeapSizePercent=80", "-Dio.netty.allocator.numDirectArenas=0", "-Dio.netty.noPreferDirect=true", "-Djava.library.path=/function"]

CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
