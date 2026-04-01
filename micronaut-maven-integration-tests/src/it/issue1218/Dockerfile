FROM container-registry.oracle.com/graalvm/native-image:25-ol9 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
COPY graalvm-reachability-metadata /home/app/graalvm-reachability-metadata
COPY native/generated /home/app/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args -H:Class=io.micronaut.build.examples.Application -H:Name=application -cp "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/"

FROM cgr.dev/chainguard/wolfi-base@sha256:a5a619c1793039dcf92f02178f37c94bb3d6001403716da59d6092dfe8d9b502
COPY --from=builder /home/app/application /app/application

EXPOSE 8080 8081
ENTRYPOINT ["/app/application"]
