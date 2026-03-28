ARG BASE_JAVA_IMAGE
FROM ${BASE_JAVA_IMAGE}
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
EXPOSE ${PORTS}
ENTRYPOINT ["java", "-cp", "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/", "${CLASS_NAME}"]
