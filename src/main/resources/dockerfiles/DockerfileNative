ARG BASE_IMAGE
ARG BASE_IMAGE_RUN
FROM ${BASE_IMAGE} AS builder
WORKDIR /home/app

COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
ARG CLASS_NAME
ARG GRAALVM_ARGS=""
RUN native-image ${GRAALVM_ARGS} -H:Class=${CLASS_NAME} -H:Name=application --no-fallback -cp "/home/app/libs/*:/home/app/classes/"

FROM ${BASE_IMAGE_RUN}
ARG EXTRA_CMD
RUN if [[ -n "${EXTRA_CMD}" ]] ; then eval ${EXTRA_CMD} ; fi
COPY --from=builder /home/app/application /app/application

ARG PORT=8080
EXPOSE ${PORT}
ENTRYPOINT ["/app/application"]
