FROM ghcr.io/graalvm/graalvm-ce:ol7-java11-22.1.0 AS builder
RUN gu install native-image

WORKDIR /
RUN curl -L -o musl.tar.gz https://more.musl.cc/10/x86_64-linux-musl/x86_64-linux-musl-native.tgz && \
    mkdir musl && tar -xzf musl.tar.gz -C musl --strip-components 1 && \
    rm -f /musl.tar.gz

ENV TOOLCHAIN_DIR="/musl"
ENV PATH="$PATH:${TOOLCHAIN_DIR}/bin"
ENV CC="${TOOLCHAIN_DIR}/bin/gcc"

RUN curl -L -o zlib.tar.gz https://zlib.net/zlib-1.2.12.tar.gz && \
    mkdir zlib && tar -xzf zlib.tar.gz -C zlib --strip-components 1 && cd zlib && \
    ./configure --static --prefix=${TOOLCHAIN_DIR} && \
    make && make install && \
    cd / && rm -rf /zlib && rm -f /zlib.tar.gz

WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image -Ob --static --libc=musl -H:Class=io.micronaut.build.examples.Application -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM scratch
COPY --from=builder /home/app/application /app/application
EXPOSE 8080
ENTRYPOINT ["/app/application"]
