FROM container-registry.oracle.com/graalvm/native-image:25-ol9 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
COPY graalvm-reachability-metadata /home/app/graalvm-reachability-metadata
COPY native/generated /home/app/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args -H:Class="io.micronaut.build.examples.Application" -H:Name=application -cp "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/"

FROM cgr.dev/chainguard/wolfi-base@sha256:52e71f61c6afd1f8d2625cff4465d8ecee156668ca665f7e9c582d1cc914eb6a
COPY --from=builder /home/app/application /app/application

EXPOSE 8080 8081
ENTRYPOINT ["/app/application"]
