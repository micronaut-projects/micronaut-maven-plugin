FROM ghcr.io/graalvm/graalvm-ce:java11-21.1.0 AS builder
RUN gu install native-image
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image -H:+StaticExecutableWithDynamicLibC -H:Class=io.micronaut.build.examples.Application -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM gcr.io/distroless/cc-debian10
COPY --from=builder /home/app/application /app/application

EXPOSE 8080
ENTRYPOINT ["/app/application"]
