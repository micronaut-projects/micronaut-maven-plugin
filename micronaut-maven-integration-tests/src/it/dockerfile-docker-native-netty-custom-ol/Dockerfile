FROM ghcr.io/graalvm/native-image-community:17-ol7 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
COPY *.args /home/app/graalvm-native-image.args
RUN native-image @/home/app/graalvm-native-image.args -H:Class=io.micronaut.build.examples.Application -H:Name=application -cp "/home/app/libs/*:/home/app/classes/"

FROM cgr.dev/chainguard/wolfi-base:latest
COPY --from=builder /home/app/application /app/application

EXPOSE 8080
ENTRYPOINT ["/app/application"]
