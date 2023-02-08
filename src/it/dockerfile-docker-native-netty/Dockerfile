FROM ghcr.io/graalvm/native-image:ol7-java11-22.2.0 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
ENV USE_NATIVE_IMAGE_JAVA_PLATFORM_MODULE_SYSTEM=false
RUN native-image -Ob -H:Class=io.micronaut.maven.examples.Application -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM frolvlad/alpine-glibc:alpine-3.12
RUN apk update && apk add libstdc++
COPY --from=builder /home/app/application /app/application

EXPOSE 8080
ENTRYPOINT ["/app/application"]
