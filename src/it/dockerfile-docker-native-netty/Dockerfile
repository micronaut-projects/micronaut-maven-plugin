FROM ghcr.io/graalvm/graalvm-ce:java11-21.0.0 AS builder
RUN gu install native-image
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image -H:Class=io.micronaut.build.examples.Application -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM frolvlad/alpine-glibc:alpine-3.12
RUN apk update && apk add libstdc++
COPY --from=builder /home/app/application /app/application

EXPOSE 8080
ENTRYPOINT ["/app/application"]
