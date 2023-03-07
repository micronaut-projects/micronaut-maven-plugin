FROM ghcr.io/graalvm/native-image:ol7-java17-22.3.0 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
COPY *.args /home/app/graalvm-native-image.args
ENV USE_NATIVE_IMAGE_JAVA_PLATFORM_MODULE_SYSTEM=false
RUN native-image @/home/app/graalvm-native-image.args -H:Class=io.micronaut.build.examples.Application -H:Name=application -cp "/home/app/libs/*:/home/app/classes/"

FROM gcr.io/distroless/cc-debian10
COPY --from=builder /home/app/application /app/application

EXPOSE 8080
ENTRYPOINT ["/app/application"]
