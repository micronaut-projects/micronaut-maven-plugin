FROM amazonlinux:2 AS graalvm
ENV LANG=en_US.UTF-8
RUN yum update -y && yum install -y gcc gcc-c++ zlib-devel zip tar gzip && yum clean all
RUN curl -4 -L https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-20.3.0/graalvm-ce-java8-linux-amd64-20.3.0.tar.gz -o /tmp/graalvm.tar.gz \
    && tar -zxf /tmp/graalvm.tar.gz -C /tmp \
    && mv /tmp/graalvm-ce-java8-20.3.0 /usr/lib/graalvm \
    && rm -rf /tmp/*
RUN /usr/lib/graalvm/bin/gu install native-image
ENV PATH=/usr/lib/graalvm/bin:${PATH}
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
RUN native-image -H:Class=io.micronaut.function.aws.runtime.MicronautLambdaRuntime -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM amazonlinux:2
WORKDIR /function
RUN yum install -y zip && yum clean all
COPY --from=graalvm /home/app/application /function/func
RUN echo "#!/bin/sh" >> bootstrap && echo "set -euo pipefail" >> bootstrap && echo "./func -Djava.library.path=$(pwd)" >> bootstrap
RUN chmod 777 bootstrap
RUN chmod 777 func
RUN zip -j function.zip bootstrap func
