FROM ghcr.io/graalvm/native-image:ol7-java17-22.3.0 AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
ENV USE_NATIVE_IMAGE_JAVA_PLATFORM_MODULE_SYSTEM=false
RUN native-image -Ob --report-unsupported-elements-at-runtime -H:Class=com.fnproject.fn.runtime.EntryPoint -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM fnproject/fn-java-fdk:jre17-latest AS fnfdk

FROM frolvlad/alpine-glibc:alpine-3.12
WORKDIR /function
COPY --from=builder /home/app/application /function/func
COPY --from=fnfdk /function/runtime/lib/* .
ENTRYPOINT ["./func", "-XX:MaximumHeapSizePercent=80", "-Djava.library.path=/function"]
CMD ["io.micronaut.oraclecloud.function.http.HttpFunction::handleRequest"]
