FROM container-registry.oracle.com/graalvm/native-image:25-ol9 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
COPY graalvm-reachability-metadata /home/app/graalvm-reachability-metadata
COPY native/generated /home/app/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args -H:Class="io.micronaut.build.examples.Application" -H:Name=application -cp "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/"

FROM cgr.dev/chainguard/wolfi-base@sha256:be019c1c74fe69204a42c4e10bc4d886a49bf5403ffa74374ef7baa408fc9a26
COPY --from=builder /home/app/application /app/application

EXPOSE 8080 8081
ENTRYPOINT ["/app/application"]
