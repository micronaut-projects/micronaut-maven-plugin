FROM ${CHECKPOINT_IMAGE} AS crac-checkpoint

FROM ubuntu:18.04

WORKDIR /home/app

# Add required libraries
RUN apt-get update && apt-get install -y \
        libnl-3-200 \
    && rm -rf /var/lib/apt/lists/*

# Copy CRaC JDK from the checkpoint image (to save a download)
COPY --from=crac-checkpoint /azul-crac-jdk /azul-crac-jdk

# Copy layers
COPY cr /home/app/cr
COPY --from=crac-checkpoint /home/app/classes /home/app/classes
COPY --from=crac-checkpoint /home/app/libs /home/app/libs
COPY scripts/run.sh /home/app/run.sh

EXPOSE 8080

ENTRYPOINT ["/home/app/run.sh"]
