FROM openjdk:15-alpine
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
EXPOSE ${PORT}
ENTRYPOINT ["java", "-cp", "/home/app/libs/*:/home/app/classes/", "${CLASS_NAME}"]